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
import Reader.FrameUI as FrameUI
import Reader.Msg as Msg exposing (Msg)
import Reader.SelectedFrameTree as SelectedFrameTree exposing (SelectedFrameTree)
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
    , hoveredExpr : Maybe TraceData.ExprWithContext
    , selectedFrames : Maybe SelectedFrameTree
    }


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
        Msg.HoverExpr exprData ->
            { model | hoveredExpr = Just exprData }

        Msg.SelectTopLevelFrame frameTrace ->
            let
                selectedFrameTree =
                    SelectedFrameTree.fromTrace (TraceData.InstrumentedFrame frameTrace)
            in
            { model | selectedFrames = Just selectedFrameTree }

        Msg.OpenChildFrame childFrameId ->
            case model.selectedFrames of
                Nothing ->
                    Debug.log "Unexpected OpenChildFrame msg!" model

                Just topLevelSelectedFrame ->
                    { model | selectedFrames = Maybe.map (SelectedFrameTree.openFrame childFrameId) model.selectedFrames }



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
                , E.onClick (Msg.SelectTopLevelFrame frame)
                ]
                [ text (SourceMap.frameIdToString frame.sourceId) ]
    in
    Flex.columnWith [ width ] <|
        List.map viewFrameLink instrumented
            ++ [ text (String.fromInt numNonInstrumented ++ " noninstrumented frames")
               ]


viewDetailsSidebar : List (Attribute Msg) -> Maybe TraceData.ExprWithContext -> Html Msg
viewDetailsSidebar layout maybeExpr =
    let
        container =
            div layout
    in
    case maybeExpr of
        Nothing ->
            container [ text "No expression selected" ]

        Just { frameSrcId, exprId, expr } ->
            case expr.value of
                Nothing ->
                    container [ text "Expression has no (recorded) value" ]

                Just val ->
                    container [ text (Value.toString val) ]


viewTraceWindow :
    Attribute Msg
    -> SourceMap
    -> Maybe TraceData.ExprWithContext
    -> Maybe SelectedFrameTree
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
                    selFrame
                        |> SelectedFrameTree.getOpenFrames
                        |> List.map (FrameUI.view sources hoveredExpr)
            in
            container childTraces
