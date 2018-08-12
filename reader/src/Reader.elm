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
import Reader.Flex as Flex
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


type ModelConsideringInit
    = ProgramDataError JD.Error
    | ProgramDataReceived Model


type alias Model =
    { sources : SourceMap
    , traces : TraceData
    , hoveredExpr : Maybe CompleteExprData
    , selectedFrames : Maybe SelectedFrame
    }


type SelectedFrame
    = SelectedFrame
        -- the selected frame:
        TraceData.Frame
        -- the open child frame, if any
        (Maybe TraceData.FrameId)
        -- all the child frames (including the open one):
        (List SelectedFrame)


frameSelectionFrom : TraceData.Frame -> SelectedFrame
frameSelectionFrom frame =
    SelectedFrame frame Nothing (List.map frameSelectionFrom (TraceData.childFrames frame))


frameSelectionId : SelectedFrame -> TraceData.FrameId
frameSelectionId (SelectedFrame frame _ _) =
    TraceData.frameIdOf frame


type alias CompleteExprData =
    ( SourceMap.FrameId, SourceMap.ExprId, TraceData.Expr )


{-| parseConfig is used by Reader.js to initialize the model from
the JSON containing source map and tracing data.
-}
parseConfig : String -> ModelConsideringInit
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
                { sources = sources
                , traces = traces
                , hoveredExpr = Nothing
                , selectedFrames = Nothing
                }



-- UPDATE


type Msg
    = HoverExpr CompleteExprData
    | SelectTopLevelFrame TraceData.InstrumentedFrameData
    | OpenChildFrame TraceData.FrameId


updateConsideringInit : Msg -> ModelConsideringInit -> ( ModelConsideringInit, Cmd Msg )
updateConsideringInit msg model =
    case model of
        ProgramDataError e ->
            ( ProgramDataError e
            , Cmd.none
            )

        ProgramDataReceived data ->
            let
                newData =
                    update msg data
            in
            ( ProgramDataReceived newData
            , Cmd.none
            )


update : Msg -> Model -> Model
update msg model =
    case msg of
        HoverExpr exprData ->
            { model | hoveredExpr = Just exprData }

        SelectTopLevelFrame frameTrace ->
            let
                selectedFrame =
                    frameSelectionFrom (TraceData.InstrumentedFrame frameTrace)
            in
            { model | selectedFrames = Just selectedFrame }

        OpenChildFrame childFrameId ->
            case model.selectedFrames of
                Nothing ->
                    Debug.log "Unexpected OpenChildFrame msg!" model

                Just topLevelSelectedFrame ->
                    { model | selectedFrames = Maybe.map (openFrame childFrameId) model.selectedFrames }


openFrame : TraceData.FrameId -> SelectedFrame -> SelectedFrame
openFrame childFrameId =
    let
        isTarget (SelectedFrame otherFrame _ _) =
            TraceData.frameIdsEqual
                childFrameId
                (TraceData.frameIdOf otherFrame)

        isAncestor =
            frameSelectionId >> TraceData.isAncestorOf childFrameId

        openFrameIn ((SelectedFrame otherFrame maybeOpenChildId children) as selectedFrame) =
            if isTarget selectedFrame then
                selectedFrame
            else
                let
                    combine sf ( newChild, restChildren ) =
                        if isTarget sf || isAncestor sf then
                            ( Just (frameSelectionId sf)
                            , openFrameIn sf :: restChildren
                            )
                        else
                            ( newChild, sf :: restChildren )

                    ( maybeNewOpenChild, newChildren ) =
                        children
                            |> List.foldr combine ( Nothing, [] )
                in
                SelectedFrame otherFrame maybeNewOpenChild newChildren
    in
    openFrameIn



-- VIEW


viewConsideringInit : ModelConsideringInit -> Html Msg
viewConsideringInit generalModel =
    case generalModel of
        ProgramDataReceived model ->
            view model

        ProgramDataError err ->
            div []
                [ pre [] [ text <| JD.errorToString err ]
                ]


view : Model -> Html Msg
view { sources, traces, hoveredExpr, selectedFrames } =
    Flex.column
        [ h2 [] [ text "Elm Reader" ]
        , Flex.row
            [ viewOutlineSidebar (A.style "flex" "2") traces
            , viewTraceWindow (A.style "flex" "5") sources hoveredExpr selectedFrames
            , viewDetailsSidebar [ A.style "flex" "3" ] hoveredExpr
            ]
        ]


fmtErr : String -> Html msg
fmtErr msg =
    pre [ A.style "white-space" "pre-wrap" ] [ text msg ]



-- VIEW: viewing a nav sidebar


viewOutlineSidebar : Attribute Msg -> TraceData -> Html Msg
viewOutlineSidebar width (TraceData frames) =
    let
        instrumented =
            frames
                |> List.filterMap
                    (\f ->
                        case f of
                            TraceData.InstrumentedFrame data ->
                                Just data

                            TraceData.NonInstrumentedFrame _ _ ->
                                Nothing
                    )

        numNonInstrumented =
            List.length frames - List.length instrumented

        viewFrameLink frame =
            a
                [ A.href "#"
                , E.onClick (SelectTopLevelFrame frame)
                ]
                [ text (SourceMap.frameIdToString frame.sourceId) ]
    in
    Flex.columnWith [ width ] <|
        List.map viewFrameLink instrumented
            ++ [ text (String.fromInt numNonInstrumented ++ " noninstrumented frames")
               ]



-- VIEW: viewing a sidebar with information about the frames


viewDetailsSidebar : List (Attribute Msg) -> Maybe CompleteExprData -> Html Msg
viewDetailsSidebar layout maybeExpr =
    let
        container =
            div layout
    in
    case maybeExpr of
        Nothing ->
            container [ text "No expression selected" ]

        Just ( frameId, id, expr ) ->
            case expr.value of
                Nothing ->
                    container [ text "Expression has no (recorded) value" ]

                Just val ->
                    container [ text (Value.toString val) ]



-- VIEW: viewing a stack of frames


stackOfOpenFrames :
    SelectedFrame
    -> List TraceData.Frame -- TODO: include more data than TraceData.Frame
stackOfOpenFrames (SelectedFrame thisFrame maybeOpenChildId children) =
    case maybeOpenChildId of
        Nothing ->
            [ thisFrame ]

        Just childId ->
            case List.filter (frameSelectionId >> TraceData.frameIdsEqual childId) children of
                [ openChild ] ->
                    thisFrame :: stackOfOpenFrames openChild

                [] ->
                    Debug.log
                        "ERROR: did not find open child ID in child frames"
                        [ thisFrame ]

                (anOpenChild :: _ :: _) as duplicates ->
                    let
                        _ =
                            Debug.log
                                "ERROR: found duplicate child frames with same ID"
                                (List.map frameSelectionId duplicates)
                    in
                    thisFrame
                        :: stackOfOpenFrames anOpenChild


viewTraceWindow :
    Attribute Msg
    -> SourceMap
    -> Maybe CompleteExprData
    -> Maybe SelectedFrame
    -> Html Msg
viewTraceWindow width sources hoveredExpr maybeTrace =
    let
        container =
            div [ width ]
    in
    case maybeTrace of
        Nothing ->
            container [ text "Select a frame to view on the left" ]

        Just selFrame ->
            let
                childTraces =
                    stackOfOpenFrames selFrame
                        |> List.map (viewTrace sources hoveredExpr)
            in
            container childTraces


viewTraceInstrumented sources hoveredExpr trace =
    viewTrace sources hoveredExpr (TraceData.InstrumentedFrame trace)


viewTrace : SourceMap -> Maybe CompleteExprData -> TraceData.Frame -> Html Msg
viewTrace srcMap hoveredExpr traceFrame =
    case traceFrame of
        TraceData.NonInstrumentedFrame runtimeId childFrames ->
            frameContainerElem
                [ text ("Non-instrumented frame (#" ++ TraceData.frameIdToString runtimeId ++ ").")
                , viewChildFrames srcMap traceFrame hoveredExpr
                ]

        TraceData.InstrumentedFrame tracedFrame ->
            let
                maybeFrame =
                    Dict.lookup tracedFrame.sourceId srcMap.frames

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

                        headerMsg =
                            p []
                                [ text "Instrumented frame (sourceId: "
                                , code [] [ text (SourceMap.frameIdToString tracedFrame.sourceId) ]
                                , text ", runtimeId: "
                                , code [] [ text (TraceData.frameIdToString tracedFrame.runtimeId) ]
                                , text ")"
                                ]
                    in
                    frameContainerElem <|
                        [ headerMsg
                        , content
                        ]

                Nothing ->
                    let
                        errMsg =
                            "failed to find frame region! id: "
                                ++ toString tracedFrame.sourceId
                                ++ "\nsrcMap: "
                                ++ toString srcMap
                    in
                    frameContainerElem [ pre [] [ text errMsg ] ]


viewChildFrames : SourceMap -> TraceData.Frame -> Maybe CompleteExprData -> Html Msg
viewChildFrames srcMap traceFrame hoveredExprId =
    let
        intoListItem elem =
            li [] [ elem ]

        children =
            TraceData.childFrames traceFrame
                |> List.map (viewTrace srcMap hoveredExprId >> intoListItem)
    in
    if children == [] then
        text "No child frames"
    else
        div []
            [ text "Child frames:"
            , ul [] children
            ]


frameContainerElem : List (Html msg) -> Html msg
frameContainerElem items =
    div
        [ A.style "border-left" "1px solid green"
        , A.style "padding-left" "3px"
        , A.style "margin-bottom" "6px"
        ]
        items



-- VIEW: viewing the contents of a frame


viewFrameTrace : SourceMap -> Maybe CompleteExprData -> TraceData.InstrumentedFrameData -> Result String (Html Msg)
viewFrameTrace srcMap hoveredExprId tracedFrame =
    frameToTokens srcMap tracedFrame.sourceId
        |> Result.andThen (viewFrameTokens tracedFrame hoveredExprId)
        |> Result.map (pre [])


type Token
    = TokenExprStart SourceMap.ExprId -- '<span title="[expr value]">'
    | TokenExprEnd SourceMap.ExprId -- '</span>'
    | TokenChar Char -- 'x'


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
                                    ( Just ( context.frameTrace.sourceId, hereExprId, expr )
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
                            (frameId == context.frameTrace.sourceId)
                                && (id == hereExprId)
                                && sameValue

                        Nothing ->
                            False
            in
            viewExprTokens { context | currentExpr = hereExprId } restTokens
                |> Result.andThen
                    (\( htmlItemsHere, tokensAfterHereExpr ) ->
                        viewExprTokens context tokensAfterHereExpr
                            |> Result.map
                                (\( htmlItemsAfterHere, tokensAfterContext ) ->
                                    ( exprElem
                                        maybeExprInfo
                                        exprTitle
                                        isHovered
                                        context.frameTrace
                                        htmlItemsHere
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
    -> TraceData.InstrumentedFrameData
    -> List (Html Msg)
    -> Html Msg
exprElem maybeExprData title isHovered frameTrace children =
    let
        handlers =
            case maybeExprData of
                Nothing ->
                    []

                Just (( _, _, expr ) as exprData) ->
                    let
                        handleMouseOver =
                            E.stopPropagationOn "mouseover"
                                (JD.succeed ( HoverExpr exprData, True ))

                        handleClick =
                            case expr.childFrame of
                                Nothing ->
                                    []

                                Just childFrame ->
                                    [ E.stopPropagationOn "click"
                                        (JD.succeed ( OpenChildFrame (TraceData.frameIdOf childFrame), True ))
                                    , A.style "cursor" "pointer"
                                    ]
                    in
                    handleMouseOver :: handleClick

        coloring =
            if isHovered then
                [ A.style "background-color" "rgb(230, 230, 200)" ]
            else
                []
    in
    span
        (coloring
            ++ handlers
            ++ [ A.title title
               , A.classList
                    [ ( "expression", True )
                    , ( "expression--hovered", isHovered )
                    ]
               ]
        )
        children
