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
import Html.Events exposing (onClick)
import Json.Decode as JD
import Reader.Dict as D
import Reader.ProgramConfig as PC
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


{-| -}
parseConfig : String -> Result JD.Error PC.Config
parseConfig =
    JD.decodeString PC.decodeConfig


main : Program () Model Msg
main =
    Elm.Kernel.Reader.main



-- MODEL


type alias Model =
    { programConfig : Result JD.Error PC.Config
    }



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update Msg ({ programConfig } as model) =
    ( model
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    case model.programConfig of
        Ok parsed ->
            viewTraces parsed

        Err err ->
            div []
                [ pre [] [ text <| JD.errorToString err ]
                ]


viewTraces : PC.Config -> Html Msg
viewTraces { sourceMap, traceData } =
    let
        (PC.TraceData traceFrames) =
            traceData
    in
    viewTrace sourceMap (PC.NonInstrumentedFrame traceFrames)


fmtErr : String -> Html msg
fmtErr msg =
    pre [ A.style "white-space" "pre-wrap" ] [ text msg ]


viewTrace : PC.SourceMap -> PC.TraceFrame -> Html Msg
viewTrace srcMap traceFrame =
    case traceFrame of
        PC.InstrumentedFrame frameId exprTraces ->
            let
                maybeFrame =
                    D.lookup frameId srcMap.frames

                maybeRegionSource =
                    maybeFrame
                        |> Maybe.map .region
                        |> Maybe.andThen (\region -> PC.lookupRegionSource region srcMap.sources)
            in
            case Maybe.map2 Tuple.pair maybeFrame maybeRegionSource of
                Just ( frame, src ) ->
                    let
                        content =
                            case viewFrameTrace srcMap frameId exprTraces of
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
                    box <|
                        [ text ("Instrumented frame (id: " ++ toString frameId ++ ")")
                        , content
                        , viewChildFrames srcMap traceFrame
                        ]

                Nothing ->
                    let
                        errMsg =
                            "failed to find frame region! id: "
                                ++ toString frameId
                                ++ "\nsrcMap: "
                                ++ toString srcMap
                    in
                    box [ pre [] [ text errMsg ] ]

        PC.NonInstrumentedFrame childFrames ->
            let
                children =
                    childFrames
                        |> List.map (viewTrace srcMap)
            in
            box
                [ text "Non-instrumented frame. "
                , viewChildFrames srcMap traceFrame
                ]


viewChildFrames : PC.SourceMap -> PC.TraceFrame -> Html Msg
viewChildFrames srcMap traceFrame =
    let
        children =
            List.map (viewTrace srcMap) (PC.childFrames traceFrame)
    in
    if children == [] then
        text "No child frames"
    else
        div []
            [ text "Child frames:"
            , ul [] children
            ]


box : List (Html msg) -> Html msg
box items =
    li
        [ A.style "border-left" "1px solid green"
        , A.style "padding-left" "3px"
        , A.style "margin-bottom" "6px"
        ]
        items


type Token
    = TokenExprStart PC.ExprId -- '<span title="[expr value]">'
    | TokenExprEnd PC.ExprId -- '</span>'
    | TokenChar Char -- 'x'


viewFrameTrace : PC.SourceMap -> PC.FrameId -> D.Dict PC.ExprId PC.TraceExpr -> Result String (Html Msg)
viewFrameTrace srcMap frameId exprTraces =
    viewFrameTokens srcMap frameId
        |> Result.andThen (viewAllFrameTokens exprTraces)
        |> Result.map (pre [])


viewAllFrameTokens : D.Dict PC.ExprId PC.TraceExpr -> List Token -> Result String (List (Html Msg))
viewAllFrameTokens exprTraces tokens =
    case tokens of
        (TokenChar ch) :: rest ->
            viewAllFrameTokens exprTraces rest
                |> Result.map (\restHtml -> text (String.fromChar ch) :: restHtml)

        (TokenExprStart exprId) :: rest ->
            case takeTokensInExpr exprTraces exprId rest of
                Ok ( htmlItems, tokensAfterExpr ) ->
                    viewAllFrameTokens exprTraces tokensAfterExpr
                        |> Result.map (\restHtml -> htmlItems ++ restHtml)

                Err e ->
                    Err (e ++ "\n All tokens were:" ++ toString tokens)

        (TokenExprEnd exprId) :: rest ->
            Err ("unexpected TokenExprEnd [\n  " ++ (String.join ",\n  " <| List.map toString tokens))

        [] ->
            Ok []


{-| takeTokensInExpr parses a token stream to Html, processing up to the end of `contextExpr`
(i.e. until the token `TokenExprEnd contextExpr`), and returns a list of Html items in that
expression as well as the list of remaining tokens.
-}
takeTokensInExpr : D.Dict PC.ExprId PC.TraceExpr -> PC.ExprId -> List Token -> Result String ( List (Html Msg), List Token )
takeTokensInExpr exprTraces contextExpr tokens =
    case tokens of
        [] ->
            Err "unexpected end of stream"

        (TokenChar ch) :: restTokens ->
            takeTokensInExpr exprTraces contextExpr restTokens
                |> Result.map
                    (\( restHtmlInContext, tokensAfterContext ) ->
                        ( text (String.fromChar ch) :: restHtmlInContext
                        , tokensAfterContext
                        )
                    )

        (TokenExprStart hereExprId) :: restTokens ->
            let
                exprTitle =
                    case D.lookup hereExprId exprTraces of
                        Nothing ->
                            "Did not find hereExprId ("
                                ++ toString hereExprId
                                ++ ") in exprTraces: "
                                ++ toString exprTraces

                        Just { value } ->
                            case value of
                                Just val ->
                                    PC.valueToString val

                                Nothing ->
                                    "<no value associated with this expr in trace frame>"
            in
            takeTokensInExpr exprTraces hereExprId restTokens
                |> Result.andThen
                    (\( htmlItemsHere, tokensAfterHereExpr ) ->
                        takeTokensInExpr exprTraces contextExpr tokensAfterHereExpr
                            |> Result.map
                                (\( htmlItemsAfterHere, tokensAfterContext ) ->
                                    ( span (A.title exprTitle :: exprStyles) htmlItemsHere
                                        :: htmlItemsAfterHere
                                    , tokensAfterContext
                                    )
                                )
                    )

        (TokenExprEnd closingExprId) :: restTokens ->
            if closingExprId == contextExpr then
                Ok ( [], restTokens )
            else
                Err
                    ("Unexpected TokenExprEnd: "
                        ++ toString closingExprId
                        ++ ", restTokens: "
                        ++ toString restTokens
                    )


exprStyles : List (Attribute msg)
exprStyles =
    [ A.style "border" "1px solid black"
    , A.style "padding" "1px"

    -- the expressions may span multiple lines, so they properly
    -- should not be inline-block, but this makes it easier to see them for
    -- the time being
    , A.style "display" "inline-block"
    ]


viewFrameTokens : PC.SourceMap -> PC.FrameId -> Result String (List Token)
viewFrameTokens srcMap frameId =
    case D.lookup frameId srcMap.frames of
        Nothing ->
            Err "could not find frameId in srcMap.frames"

        Just { region, exprRegions } ->
            case PC.lookupRegionSource region srcMap.sources of
                Nothing ->
                    Err "could not find frame.region in srcMap.sources"

                Just src ->
                    Ok (viewFrameSrcTokens region.start src exprRegions)


viewFrameSrcTokens : PC.Position -> String -> D.Dict PC.ExprId (List PC.Region) -> List Token
viewFrameSrcTokens initialPos src exprRegions =
    let
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
                            PC.exprsStartingHere position exprRegions
                                |> List.map TokenExprStart

                        ends =
                            PC.exprsEndingHere nextPos exprRegions
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


rconcat : List (List a) -> List a
rconcat =
    List.reverse >> List.concat
