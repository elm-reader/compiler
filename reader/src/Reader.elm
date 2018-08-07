module Reader
    exposing
        ( markInstrumented
        , parseConfig
        , recordCall
        , recordExpr
        , recordFrame
        , seq
        )

{-|

    Reader.

    @docs recordExpr, recordCall, recordFrame, markInstrumented, seq, parseConfig

-}

import Browser
import Debug exposing (toString)
import Elm.Kernel.Reader
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData as TraceData exposing (TraceData(..))
import Reader.TraceData.Value as Value exposing (Value)
import Reader.Utils as Utils


{-| -}
recordExpr : Int -> a -> a
recordExpr =
    Elm.Kernel.Reader.recordExpr


{-| -}
recordCall : Int -> (a -> b) -> (() -> c) -> c
recordCall =
    Elm.Kernel.Reader.recordCall


{-| -}
recordFrame : String -> (() -> a) -> a
recordFrame =
    Elm.Kernel.Reader.recordFrame


{-| -}
markInstrumented : (a -> b) -> (a -> b)
markInstrumented =
    Elm.Kernel.Reader.markInstrumented


{-| -}
seq : a -> b -> b
seq =
    Elm.Kernel.Reader.seq


main : Program () Model Msg
main =
    Elm.Kernel.Reader.main



-- MODEL


type Model
    = ProgramDataError JD.Error
    | ProgramDataReceived
        { sources : SourceMap
        , traces : TraceData
        , hoveredExpr : Maybe CompleteExprData
        }


type alias CompleteExprData =
    ( SourceMap.FrameId, SourceMap.ExprId, TraceData.Expr )


{-| parseConfig is used by Reader.js to initialize the model from
the JSON containing source map and tracing data.
-}
parseConfig : String -> Model
parseConfig data =
    let
        decode =
            JD.map2 (\src traces -> { sources = src, traces = traces })
                (JD.field "source_map" SourceMap.decode)
                (JD.field "traces" TraceData.decode)
    in
    case JD.decodeString decode data of
        Err e ->
            ProgramDataError e

        Ok { sources, traces } ->
            ProgramDataReceived
                { sources = sources, traces = traces, hoveredExpr = Nothing }



-- UPDATE


type Msg
    = HoverExpr CompleteExprData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ProgramDataError e ->
            ( ProgramDataError e
            , Cmd.none
            )

        ProgramDataReceived data ->
            case msg of
                HoverExpr exprData ->
                    ( ProgramDataReceived { data | hoveredExpr = Just exprData }
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        ProgramDataReceived { sources, traces, hoveredExpr } ->
            viewTraces sources traces hoveredExpr

        ProgramDataError err ->
            div []
                [ pre [] [ text <| JD.errorToString err ]
                ]


viewTraces : SourceMap -> TraceData -> Maybe CompleteExprData -> Html Msg
viewTraces sourceMap (TraceData traceFrames) hoveredExpr =
    viewTrace sourceMap hoveredExpr (TraceData.NonInstrumentedFrame traceFrames)


fmtErr : String -> Html msg
fmtErr msg =
    pre [ A.style "white-space" "pre-wrap" ] [ text msg ]


viewTrace : SourceMap -> Maybe CompleteExprData -> TraceData.Frame -> Html Msg
viewTrace srcMap hoveredExpr traceFrame =
    case traceFrame of
        TraceData.NonInstrumentedFrame childFrames ->
            frameElem
                [ text "Non-instrumented frame. "
                , viewChildFrames srcMap traceFrame hoveredExpr
                ]

        TraceData.InstrumentedFrame tracedFrame ->
            let
                maybeFrame =
                    Dict.lookup tracedFrame.id srcMap.frames

                maybeRegionSource =
                    maybeFrame
                        |> Maybe.map .region
                        |> Maybe.andThen (\region -> SourceMap.lookupRegionSource region srcMap.sources)
            in
            case Maybe.map2 Tuple.pair maybeFrame maybeRegionSource of
                Just ( frame, src ) ->
                    let
                        content =
                            case viewFrameTrace srcMap hoveredExpr tracedFrame of
                                Err e ->
                                    div []
                                        [ p [ A.style "font-weight" "bold" ] [ text "Got error:" ]
                                        , fmtErr e
                                        , p [ A.style "font-weight" "bold" ] [ text "Parsing this frame's trace:" ]
                                        , pre [] [ text src ]
                                        ]

                                Ok frameTraceHtml ->
                                    frameTraceHtml
                    in
                    frameElem <|
                        [ text ("Instrumented frame (id: " ++ toString tracedFrame.id ++ ")")
                        , content
                        , viewChildFrames srcMap traceFrame hoveredExpr
                        ]

                Nothing ->
                    let
                        errMsg =
                            "failed to find frame region! id: "
                                ++ toString tracedFrame.id
                                ++ "\nsrcMap: "
                                ++ toString srcMap
                    in
                    frameElem [ pre [] [ text errMsg ] ]


viewChildFrames : SourceMap -> TraceData.Frame -> Maybe CompleteExprData -> Html Msg
viewChildFrames srcMap traceFrame hoveredExprId =
    let
        children =
            List.map (viewTrace srcMap hoveredExprId) (TraceData.childFrames traceFrame)
    in
    if children == [] then
        text "No child frames"
    else
        div []
            [ text "Child frames:"
            , ul [] children
            ]


frameElem : List (Html msg) -> Html msg
frameElem items =
    li
        [ A.style "border-left" "1px solid green"
        , A.style "padding-left" "3px"
        , A.style "margin-bottom" "6px"
        ]
        items


type Token
    = TokenExprStart SourceMap.ExprId -- '<span title="[expr value]">'
    | TokenExprEnd SourceMap.ExprId -- '</span>'
    | TokenChar Char -- 'x'


viewFrameTrace : SourceMap -> Maybe CompleteExprData -> TraceData.InstrumentedFrameData -> Result String (Html Msg)
viewFrameTrace srcMap hoveredExprId tracedFrame =
    frameToTokens srcMap tracedFrame.id
        |> Result.andThen (viewFrameTokens tracedFrame hoveredExprId)
        |> Result.map (pre [])


frameToTokens : SourceMap -> SourceMap.FrameId -> Result String (List Token)
frameToTokens srcMap frameId =
    case Dict.lookup frameId srcMap.frames of
        Nothing ->
            Err "could not find frameId in srcMap.frames"

        Just { region, exprRegions } ->
            case SourceMap.lookupRegionSource region srcMap.sources of
                Nothing ->
                    Err "could not find frame.region in srcMap.sources"

                Just src ->
                    Ok (frameSrcToTokens region.start src exprRegions)


frameSrcToTokens : SourceMap.Position -> String -> Dict SourceMap.ExprId (List SourceMap.Region) -> List Token
frameSrcToTokens initialPos src exprRegions =
    let
        rconcat : List (List a) -> List a
        rconcat =
            List.reverse >> List.concat

        -- TODO: take finalPos instead of initialPos, to change this to use
        -- String.foldr and avoid the double-reversal
        ( _, reversedAssociation ) =
            String.foldl
                (\char ( position, accum ) ->
                    let
                        nextPos =
                            if char == '\n' then
                                { line = position.line + 1
                                , col = 1
                                }
                            else
                                { line = position.line
                                , col = position.col + 1
                                }

                        starts =
                            SourceMap.exprsStartingAt position exprRegions
                                |> List.map TokenExprStart

                        ends =
                            SourceMap.exprsEndingAt nextPos exprRegions
                                |> List.map TokenExprEnd
                    in
                    ( nextPos
                    , rconcat
                        [ accum
                        , starts
                        , [ TokenChar char ]
                        , ends
                        ]
                    )
                )
                ( initialPos, [] )
                src
    in
    List.reverse reversedAssociation


viewFrameTokens : TraceData.InstrumentedFrameData -> Maybe CompleteExprData -> List Token -> Result String (List (Html Msg))
viewFrameTokens frameTrace hoveredExpr tokens =
    case tokens of
        (TokenChar ch) :: rest ->
            viewFrameTokens frameTrace hoveredExpr rest
                |> Result.map (\restHtml -> text (String.fromChar ch) :: restHtml)

        (TokenExprStart exprId) :: rest ->
            let
                exprRenderingContext =
                    { frameTrace = frameTrace
                    , currentExpr = exprId
                    , hoveredExpr = hoveredExpr
                    }
            in
            case viewExprTokens exprRenderingContext rest of
                Ok ( htmlItems, tokensAfterExpr ) ->
                    viewFrameTokens frameTrace hoveredExpr tokensAfterExpr
                        |> Result.map (\restHtml -> htmlItems ++ restHtml)

                Err e ->
                    Err (e ++ "\n All tokens were:" ++ toString tokens)

        (TokenExprEnd exprId) :: rest ->
            Err ("unexpected TokenExprEnd [\n  " ++ (String.join ",\n  " <| List.map toString tokens))

        [] ->
            Ok []


type alias ExprRenderingContext =
    { hoveredExpr : Maybe CompleteExprData
    , currentExpr : SourceMap.ExprId
    , frameTrace : TraceData.InstrumentedFrameData
    }


{-| viewExprTokens parses a token stream to Html, processing up to the end of
`context.currentExpr` (i.e. until the token `TokenExprEnd context.currentExpr`),
and returns a list of Html items in that expression as well as the list of unconsumed tokens.
-}
viewExprTokens : ExprRenderingContext -> List Token -> Result String ( List (Html Msg), List Token )
viewExprTokens context tokens =
    case tokens of
        [] ->
            Err "unexpected end of stream"

        (TokenChar ch) :: restTokens ->
            viewExprTokens context restTokens
                |> Result.map
                    (\( restHtmlInContext, tokensAfterContext ) ->
                        ( text (String.fromChar ch) :: restHtmlInContext
                        , tokensAfterContext
                        )
                    )

        (TokenExprStart hereExprId) :: restTokens ->
            let
                ( maybeExprInfo, exprTitle ) =
                    case Dict.lookup hereExprId context.frameTrace.exprs of
                        Nothing ->
                            ( Nothing
                            , "Did not find hereExprId ("
                                ++ toString hereExprId
                                ++ ") in exprTraces: "
                                ++ toString context.frameTrace.exprs
                            )

                        Just expr ->
                            case expr.value of
                                Just val ->
                                    ( Just ( context.frameTrace.id, hereExprId, expr )
                                    , Value.toString val
                                    )

                                Nothing ->
                                    ( Nothing
                                    , "<no value associated with this expr in trace frame>"
                                    )

                isHovered =
                    case context.hoveredExpr of
                        Just ( frameId, id, { value } ) ->
                            let
                                hereValueMaybe =
                                    Dict.lookup id context.frameTrace.exprs
                                        |> Maybe.andThen .value

                                sameValue =
                                    case ( value, hereValueMaybe ) of
                                        ( Just hoveredVal, Just hereVal ) ->
                                            Value.isEqual hoveredVal hereVal

                                        ( _, _ ) ->
                                            False
                            in
                            frameId == context.frameTrace.id && id == hereExprId && sameValue

                        Nothing ->
                            False
            in
            viewExprTokens { context | currentExpr = hereExprId } restTokens
                |> Result.andThen
                    (\( htmlItemsHere, tokensAfterHereExpr ) ->
                        viewExprTokens context tokensAfterHereExpr
                            |> Result.map
                                (\( htmlItemsAfterHere, tokensAfterContext ) ->
                                    ( exprElem maybeExprInfo exprTitle isHovered htmlItemsHere
                                        :: htmlItemsAfterHere
                                    , tokensAfterContext
                                    )
                                )
                    )

        (TokenExprEnd closingExprId) :: restTokens ->
            if closingExprId == context.currentExpr then
                Ok ( [], restTokens )
            else
                Err
                    ("Unexpected TokenExprEnd: "
                        ++ toString closingExprId
                        ++ ", restTokens: "
                        ++ toString restTokens
                    )


exprElem :
    Maybe ( SourceMap.FrameId, SourceMap.ExprId, TraceData.Expr )
    -> String
    -> Bool
    -> List (Html Msg)
    -> Html Msg
exprElem maybeExprData title isHovered children =
    let
        handleHover =
            case maybeExprData of
                Just exprData ->
                    [ E.stopPropagationOn "mouseover"
                        (JD.succeed ( HoverExpr exprData, True ))
                    ]

                Nothing ->
                    []

        coloring =
            if isHovered then
                [ A.style "background-color" "rgb(230, 230, 200)" ]
            else
                []
    in
    span
        (coloring
            ++ handleHover
            ++ [ A.title title
               , A.classList
                    [ ( "expression", True )
                    , ( "expression--hovered", isHovered )
                    ]
               ]
        )
        children
