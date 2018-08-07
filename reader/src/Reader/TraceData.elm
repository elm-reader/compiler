module Reader.TraceData
    exposing
        ( Expr
        , Frame(..)
        , InstrumentedFrameData
        , TraceData(..)
        , childFrames
        , decode
        , isFrameInstrumented
        )

import Debug
import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData.Value as Value exposing (Value)
import Tuple


-- TRACE DATA


type TraceData
    = TraceData (List Frame)


decode : JD.Decoder TraceData
decode =
    JD.map TraceData <| JD.list decodeFrameTrace



-- FRAME TRACES


type Frame
    = InstrumentedFrame InstrumentedFrameData
    | NonInstrumentedFrame (List Frame)


type alias InstrumentedFrameData =
    { id : SourceMap.FrameId
    , exprs : Dict SourceMap.ExprId Expr
    }


isFrameInstrumented : Frame -> Bool
isFrameInstrumented frame =
    case frame of
        InstrumentedFrame _ ->
            True

        NonInstrumentedFrame _ ->
            False


decodeFrameTrace : JD.Decoder Frame
decodeFrameTrace =
    let
        decFrame =
            JD.lazy (\() -> decodeFrameTrace)

        decExpr =
            JD.lazy (\() -> decodeExpr)

        decodeInstrumented =
            JD.map2 (\id exprs -> InstrumentedFrame (InstrumentedFrameData id exprs))
                (JD.field "id" SourceMap.decodeFrameId)
                (JD.field "exprs" <| Dict.decode ( "id", SourceMap.decodeExprId ) ( "expr", decExpr ))

        decodeNonInstrumented =
            JD.map NonInstrumentedFrame
                (JD.field "child_frames" <| JD.list decFrame)
    in
    JD.oneOf [ decodeNonInstrumented, decodeInstrumented ]


childFrames : Frame -> List Frame
childFrames frameTrace =
    case frameTrace of
        InstrumentedFrame { exprs } ->
            Dict.values exprs
                |> List.map .childFrame
                |> filterListForJust

        NonInstrumentedFrame children ->
            children


filterListForJust : List (Maybe a) -> List a
filterListForJust =
    List.foldr
        (\maybeVal accum ->
            case maybeVal of
                Nothing ->
                    accum

                Just elem ->
                    elem :: accum
        )
        []



-- EXPRESSION TRACES


{-| -}
type alias Expr =
    { value : Maybe Value
    , childFrame : Maybe Frame -- frame that created the expression
    }


decodeExpr : JD.Decoder Expr
decodeExpr =
    JD.map2 Expr
        (JD.maybe <| JD.field "val" Value.decode)
        (JD.field "child_frame" <| JD.nullable decodeFrameTrace)
