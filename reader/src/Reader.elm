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
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Reader.Dict as Dict
import Reader.ProgramConfig as PC


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
        PC.InstrumentedFrame frameId exprs ->
            let
                maybeFrame =
                    Dict.lookup frameId srcMap.frames

                maybeRegionSource =
                    maybeFrame
                        |> Maybe.map .region
                        |> Maybe.andThen (\region -> PC.lookupRegionSource region srcMap.sources)
            in
            case Maybe.map2 Tuple.pair maybeFrame maybeRegionSource of
                Just ( frame, src ) ->
                    box <|
                        [ text ("Instrumented frame (id: " ++ toString frameId ++ ")")
                        , pre [] [ text src ]
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
        [ style "border-left" "1px solid green"
        , style "padding-left" "3px"
        , style "margin-bottom" "6px"
        ]
        items
