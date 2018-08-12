module Reader.TraceData
    exposing
        ( Expr
        , Frame(..)
        , FrameId(..)
        , InstrumentedFrameData
        , TraceData(..)
        , childFrames
        , decode
        , frameIdOf
        , frameIdToString
        , frameIdsEqual
        , isAncestorOf
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
    | NonInstrumentedFrame FrameId (List Frame)


type FrameId
    = -- uid: the unique ID of this runtime frame
      -- id path: the IDs of this frame's ancestors, from the root to this frame
      FrameId Int (List Int)


decodeFrameId : JD.Decoder FrameId
decodeFrameId =
    JD.map2 FrameId
        (JD.field "uid" JD.int)
        (JD.field "id_path" (JD.list JD.int))


frameIdToString : FrameId -> String
frameIdToString (FrameId uid ancestors) =
    "Frame "
        ++ String.fromInt uid
        ++ " (ancestors: ["
        ++ String.join ", " (List.map String.fromInt ancestors)
        ++ ")]"


type alias InstrumentedFrameData =
    { sourceId : SourceMap.FrameId
    , runtimeId : FrameId
    , exprs : Dict SourceMap.ExprId Expr
    }


isFrameInstrumented : Frame -> Bool
isFrameInstrumented frame =
    case frame of
        InstrumentedFrame _ ->
            True

        NonInstrumentedFrame _ _ ->
            False


frameIdOf : Frame -> FrameId
frameIdOf frame =
    case frame of
        NonInstrumentedFrame id _ ->
            id

        InstrumentedFrame { runtimeId } ->
            runtimeId


frameIdsEqual : FrameId -> FrameId -> Bool
frameIdsEqual (FrameId uid1 _) (FrameId uid2 _) =
    uid1 == uid2


{-| `isAncestorOf a b` answers whether b is an ancestor of a.

    List.filter (isAncestorOf child) frames

-}
isAncestorOf : FrameId -> FrameId -> Bool
isAncestorOf (FrameId _ ancestors) (FrameId possibleAncestor _) =
    List.member possibleAncestor ancestors


decodeFrameTrace : JD.Decoder Frame
decodeFrameTrace =
    let
        decFrame =
            JD.lazy (\() -> decodeFrameTrace)

        decExpr =
            JD.lazy (\() -> decodeExpr)

        decodeInstrumented =
            JD.map3 (\sid rid exprs -> InstrumentedFrame (InstrumentedFrameData sid rid exprs))
                (JD.field "source_map_id" SourceMap.decodeFrameId)
                (JD.field "runtime_id" decodeFrameId)
                (JD.field "exprs" <| Dict.decode ( "id", SourceMap.decodeExprId ) ( "expr", decExpr ))

        decodeNonInstrumented =
            JD.map2 NonInstrumentedFrame
                (JD.field "runtime_id" decodeFrameId)
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

        NonInstrumentedFrame _ children ->
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
