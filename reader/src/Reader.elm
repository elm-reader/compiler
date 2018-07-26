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
                    box <|
                        [ text ("Instrumented frame (id: " ++ toString frameId ++ ")")
                        , viewFrameTrace srcMap frameId exprTraces
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


viewFrameTrace : PC.SourceMap -> PC.FrameId -> D.Dict PC.ExprId PC.TraceExpr -> Html Msg
viewFrameTrace srcMap frameId exprTraces =
    viewFrameTokens srcMap frameId
        |> viewAllFrameTokens exprTraces
        |> pre []


viewAllFrameTokens : D.Dict PC.ExprId PC.TraceExpr -> List Token -> List (Html Msg)
viewAllFrameTokens exprTraces tokens =
    case tokens of
        (TokenChar ch) :: rest ->
            text (String.fromChar ch) :: viewAllFrameTokens exprTraces rest

        (TokenExprStart exprId) :: rest ->
            let
                ( htmlItems, tokensAfterExpr ) =
                    takeTokensInExpr exprTraces exprId rest
            in
            htmlItems ++ viewAllFrameTokens exprTraces tokensAfterExpr

        (TokenExprEnd exprId) :: rest ->
            Debug.todo <| "unexpected TokenExprEnd [\n  " ++ (String.join ",\n  " <| List.map toString tokens)

        [] ->
            []


{-| takeTokensInExpr parses a token stream to Html, processing up to the end of `contextExpr`
(i.e. until the token `TokenExprEnd contextExpr`), and returns a list of Html items in that
expression as well as the list of remaining tokens.
-}
takeTokensInExpr : D.Dict PC.ExprId PC.TraceExpr -> PC.ExprId -> List Token -> ( List (Html Msg), List Token )
takeTokensInExpr exprTraces contextExpr tokens =
    case tokens of
        [] ->
            Debug.todo "unexpected end of stream"

        (TokenChar ch) :: restTokens ->
            let
                ( restHtmlInContext, tokensAfterContext ) =
                    takeTokensInExpr exprTraces contextExpr restTokens
            in
            ( text (String.fromChar ch) :: restHtmlInContext
            , tokensAfterContext
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
                            toString value

                ( htmlItemsHere, tokensAfterHereExpr ) =
                    takeTokensInExpr exprTraces hereExprId restTokens

                ( htmlItemsAfterHere, tokensAfterContext ) =
                    takeTokensInExpr exprTraces contextExpr tokensAfterHereExpr
            in
            ( span (A.title exprTitle :: exprStyles) htmlItemsHere
                :: htmlItemsAfterHere
            , tokensAfterContext
            )

        (TokenExprEnd closingExprId) :: restTokens ->
            if closingExprId == contextExpr then
                ( [], restTokens )
            else
                -- FIXME
                -- Debug.todo <|
                --     "FIXME 3 unexpected TokenExprEnd: "
                --         ++ toString closingExprId
                --         ++ ", restTokens: "
                --         ++ toString restTokens
                ( [], restTokens )


exprStyles : List (Attribute msg)
exprStyles =
    [ A.style "border" "1px solid black"
    , A.style "padding" "1px"

    -- the expressions may span multiple lines, so they properly
    -- should not be inline-block, but this makes it easier to see them for
    -- the time being
    , A.style "display" "inline-block"
    ]


viewFrameTokens : PC.SourceMap -> PC.FrameId -> List Token
viewFrameTokens srcMap frameId =
    case D.lookup frameId srcMap.frames of
        Nothing ->
            Debug.todo "could not find frameId in srcMap.frames"

        Just { region, exprRegions } ->
            case PC.lookupRegionSource region srcMap.sources of
                Nothing ->
                    Debug.todo "could not find frame.region in srcMap.sources"

                Just src ->
                    viewFrameSrcTokens region.start src exprRegions


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
                                |> Utils.reverseSortWith PC.compareExprIds
                                |> List.map TokenExprStart

                        ends =
                            PC.exprsEndingHere nextPos exprRegions
                                |> List.sortWith PC.compareExprIds
                                |> List.map TokenExprEnd
                    in
                    ( nextPos
                    , ends ++ [ TokenChar char ] ++ starts ++ accum
                    )
                )
                ( initialPos, [] )
                src
    in
    List.reverse reversedAssociation
