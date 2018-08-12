module Reader.FrameUI.View.Instrumented exposing (viewFrameTrace)

{-| Reader.FrameUI.View.Instrumented handles the creation of the clickable,
highlightable text of a frame.
-}

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Reader.Msg as Msg exposing (Msg)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData as TraceData exposing (TraceData)
import Reader.TraceData.Value as Value exposing (Value)


viewFrameTrace :
    SourceMap
    -> Maybe TraceData.ExprWithContext
    -> TraceData.InstrumentedFrameData
    -> Result String (Html Msg)
viewFrameTrace srcMap hoveredExprId tracedFrame =
    frameToTokens srcMap tracedFrame.sourceId
        |> Result.andThen (viewFrameTokens tracedFrame hoveredExprId)
        |> Result.map (H.pre [])


type Token
    = TokenExprStart SourceMap.ExprId -- '<span title="[expr value]">'
    | TokenExprEnd SourceMap.ExprId -- '</span>'
    | TokenChar Char -- 'x'


frameToTokens :
    SourceMap
    -> SourceMap.FrameId
    -> Result String (List Token)
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


frameSrcToTokens :
    SourceMap.Position
    -> String
    -> Dict SourceMap.ExprId (List SourceMap.Region)
    -> List Token
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


viewFrameTokens :
    TraceData.InstrumentedFrameData
    -> Maybe TraceData.ExprWithContext
    -> List Token
    -> Result String (List (Html Msg))
viewFrameTokens frameTrace hoveredExpr tokens =
    case tokens of
        (TokenChar ch) :: rest ->
            viewFrameTokens frameTrace hoveredExpr rest
                |> Result.map (\restHtml -> H.text (String.fromChar ch) :: restHtml)

        (TokenExprStart exprId) :: rest ->
            let
                exprRenderingContext =
                    { frameTrace = frameTrace
                    , currentExpr = exprId
                    , hoveredExpr = hoveredExpr
                    }
            in
            case takeExprTokensToHtml exprRenderingContext rest of
                Ok ( htmlItems, tokensAfterExpr ) ->
                    viewFrameTokens frameTrace hoveredExpr tokensAfterExpr
                        |> Result.map (\restHtml -> htmlItems ++ restHtml)

                Err e ->
                    Err (e ++ "\n All tokens were:" ++ Debug.toString tokens)

        (TokenExprEnd exprId) :: rest ->
            Err ("unexpected TokenExprEnd [\n  " ++ (String.join ",\n  " <| List.map Debug.toString tokens))

        [] ->
            Ok []


type alias ExprRenderingContext =
    { hoveredExpr : Maybe TraceData.ExprWithContext
    , currentExpr : SourceMap.ExprId
    , frameTrace : TraceData.InstrumentedFrameData
    }


{-| takeExprTokensToHtml parses a token stream to Html, processing up to the end of
`context.currentExpr` (i.e. until the token `TokenExprEnd context.currentExpr`),
and returns a list of Html items in that expression as well as the list of unconsumed tokens.
-}
takeExprTokensToHtml :
    ExprRenderingContext
    -> List Token
    -> Result String ( List (Html Msg), List Token )
takeExprTokensToHtml context tokens =
    case tokens of
        [] ->
            Err "unexpected end of stream"

        (TokenChar ch) :: restTokens ->
            takeExprTokensToHtml context restTokens
                |> Result.map
                    (\( restHtmlInContext, tokensAfterContext ) ->
                        ( H.text (String.fromChar ch) :: restHtmlInContext
                        , tokensAfterContext
                        )
                    )

        (TokenExprStart hereExprId) :: restTokens ->
            let
                ( maybeExprData, exprTitle ) =
                    case Dict.lookup hereExprId context.frameTrace.exprs of
                        Nothing ->
                            ( Nothing
                            , "Did not find hereExprId ("
                                ++ Debug.toString hereExprId
                                ++ ") in exprTraces: "
                                ++ Debug.toString context.frameTrace.exprs
                            )

                        Just expr ->
                            case expr.value of
                                Just val ->
                                    ( Just
                                        { frameSrcId = context.frameTrace.sourceId
                                        , stackFrameId = context.frameTrace.runtimeId
                                        , exprId = hereExprId
                                        , expr = expr
                                        }
                                    , Value.toString val
                                    )

                                Nothing ->
                                    ( Nothing
                                    , "<no value associated with this expr in trace frame>"
                                    )

                highlightState =
                    case context.hoveredExpr of
                        Just hovered ->
                            let
                                isHovered =
                                    (hovered.frameSrcId == context.frameTrace.sourceId)
                                        && (hovered.exprId == hereExprId)
                                        && (hovered.stackFrameId == context.frameTrace.runtimeId)
                            in
                            if isHovered then
                                Hovered
                            else
                                NoHighlight

                        Nothing ->
                            NoHighlight

                exprOptions =
                    { title = exprTitle
                    , maybeExprData = maybeExprData
                    , highlightState = highlightState
                    , contents = [] -- to be filled in below
                    }
            in
            takeExprTokensToHtml { context | currentExpr = hereExprId } restTokens
                |> Result.andThen
                    (\( htmlItemsHere, tokensAfterHereExpr ) ->
                        takeExprTokensToHtml context tokensAfterHereExpr
                            |> Result.map
                                (\( htmlItemsAfterHere, tokensAfterContext ) ->
                                    ( exprElem { exprOptions | contents = htmlItemsHere }
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
                        ++ Debug.toString closingExprId
                        ++ ", restTokens: "
                        ++ Debug.toString restTokens
                    )


type alias ExprOptions =
    { title : String
    , maybeExprData : Maybe TraceData.ExprWithContext
    , highlightState : HighlightState
    , contents : List (Html Msg)
    }


type HighlightState
    = Hovered
    | NoHighlight


highlightStateStyles hs =
    case hs of
        Hovered ->
            [ A.style "background-color" "rgb(230, 230, 200)" ]

        NoHighlight ->
            []


exprElem : ExprOptions -> Html Msg
exprElem { title, maybeExprData, highlightState, contents } =
    let
        handlers =
            case maybeExprData of
                Nothing ->
                    []

                Just exprData ->
                    let
                        handleMouseOver =
                            E.stopPropagationOn "mouseover"
                                (JD.succeed ( Msg.HoverExpr exprData, True ))

                        handleClick =
                            case exprData.expr.childFrame of
                                Nothing ->
                                    []

                                Just childFrame ->
                                    [ E.stopPropagationOn "click"
                                        (JD.succeed ( Msg.OpenChildFrame (TraceData.frameIdOf childFrame), True ))
                                    , A.style "cursor" "pointer"
                                    ]
                    in
                    handleMouseOver :: handleClick
    in
    H.span
        (highlightStateStyles highlightState
            ++ handlers
            ++ [ A.title title ]
        )
        contents
