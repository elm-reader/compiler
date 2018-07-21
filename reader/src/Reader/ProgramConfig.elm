module Reader.ProgramConfig
    exposing
        ( Config
        , ExprId
        , Frame
        , FrameId
        , Interface
        , Interfaces
        , SourceMap
        , SourceMaps
        , TraceData(..)
        , decodeConfig
        , decodeTraceData
        , emptyInterfaces
        , emptySourceMaps
        )

import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Tuple


-- PROGRAM CONFIG


type alias Config =
    { interfaces : Interfaces, sourceMaps : SourceMaps, traceData : TraceData }


decodeConfig : JD.Decoder Config
decodeConfig =
    JD.map3 Config
        (JD.field "interfaces" decodeInterfaces)
        (JD.field "source_maps" decodeSourceMaps)
        (JD.field "traces" decodeTraceData)


type alias PackageId =
    { author : String
    , project : String
    }


decodePackage : JD.Decoder PackageId
decodePackage =
    JD.map2 PackageId
        (JD.field "author" JD.string)
        (JD.field "project" JD.string)


type alias ModuleId =
    { package : PackageId
    , mod : Name
    }


decodeModule : JD.Decoder ModuleId
decodeModule =
    JD.map2 ModuleId
        (JD.field "package" decodePackage)
        (JD.field "module" JD.string)


type alias Name =
    String



-- INTERFACES


type alias Interfaces =
    Dict ModuleId Interface


emptyInterfaces =
    Dict.empty


decodeInterfaces =
    JD.succeed Dict.empty


type alias Interface =
    ()



-- SOURCE MAPS


type alias SourceMaps =
    Dict ModuleId SourceMap


emptySourceMaps =
    Dict.empty


decodeSourceMaps : JD.Decoder SourceMaps
decodeSourceMaps =
    decodeDict
        ( "module", decodeModule )
        ( "source_map", decodeSourceMap )


type alias SourceMap =
    { source : String
    , frames : Dict FrameId Frame
    }


decodeSourceMap : JD.Decoder SourceMap
decodeSourceMap =
    JD.map2 SourceMap
        (JD.field "source" JD.string)
        (JD.field "frames" <| decodeDict ( "id", decodeFrameId ) ( "frame", decodeFrame ))


type alias FrameId =
    { mod : ModuleId
    , def : Name
    , frameIndex : Int
    }


decodeFrameId : JD.Decoder FrameId
decodeFrameId =
    JD.map3 FrameId
        (JD.field "module" decodeModule)
        (JD.field "def" JD.string)
        (JD.field "frame_index" JD.int)


type alias Frame =
    { region : Region
    , exprRegions : Dict ExprId (List Region)
    , exprNames : Dict ExprId ( ModuleId, Name )
    }


decodeFrame : JD.Decoder Frame
decodeFrame =
    let
        decodeExprName =
            JD.map2 Tuple.pair
                (JD.field "module" decodeModule)
                (JD.field "name" JD.string)
    in
    JD.map3 Frame
        (JD.field "region" decodeRegion)
        (JD.field "expr_regions" <| decodeDict ( "id", decodeExprId ) ( "regions", JD.list decodeRegion ))
        (JD.field "expr_names" <| decodeDict ( "id", decodeExprId ) ( "qualified_name", decodeExprName ))


type ExprId
    = ExprId Int


decodeExprId : JD.Decoder ExprId
decodeExprId =
    JD.map ExprId JD.int



-- REGION


type alias Region =
    { start : Position
    , cols : Position
    }


decodeRegion : JD.Decoder Region
decodeRegion =
    JD.map2 Region
        (JD.field "start" decodePosition)
        (JD.field "end" decodePosition)


type alias Position =
    { line : Int
    , col : Int
    }


decodePosition : JD.Decoder Position
decodePosition =
    JD.map2 Position
        (JD.field "line" JD.int)
        (JD.field "column" JD.int)



-- GENERAL DECODE UTILS


decodeDict : ( String, JD.Decoder key ) -> ( String, JD.Decoder value ) -> JD.Decoder (Dict key value)
decodeDict ( keyName, decodeKey ) ( valName, decodeVal ) =
    let
        decodeEntry =
            JD.map2 Tuple.pair
                (JD.field keyName decodeKey)
                (JD.field valName decodeVal)
    in
    JD.map Dict.fromList (JD.list decodeEntry)



-- TRACE DATA


type TraceData
    = TraceData (List TraceFrame)


decodeTraceData : JD.Decoder TraceData
decodeTraceData =
    JD.map TraceData <| JD.list decodeTraceFrame


{-| -}
type TraceFrame
    = InstrumentedFrame FrameId (Dict ExprId TraceExpr)
    | NonInstrumentedFrame (List TraceFrame)


decodeTraceFrame : JD.Decoder TraceFrame
decodeTraceFrame =
    let
        decFrame =
            JD.lazy (\() -> decodeTraceFrame)

        decExpr =
            JD.lazy (\() -> decodeTraceExpr)

        decodeInstrumented =
            JD.map2 InstrumentedFrame
                (JD.field "id" decodeFrameId)
                (JD.field "exprs" <| decodeDict ( "id", decodeExprId ) ( "expr", decExpr ))

        decodeNonInstrumented =
            JD.map NonInstrumentedFrame
                (JD.field "child_frames" <| JD.list decFrame)
    in
    JD.oneOf [ decodeNonInstrumented, decodeInstrumented ]


{-| -}
type alias TraceExpr =
    { value : Maybe Value
    , childFrame : Maybe TraceFrame -- frame that created the expression
    }


decodeTraceExpr : JD.Decoder TraceExpr
decodeTraceExpr =
    JD.map2 TraceExpr
        (JD.maybe <| JD.field "val" decodeValue)
        (JD.field "child_frame" <| JD.nullable decodeTraceFrame)



-- An Elm value


type Value
    = Num Float
    | Str String
    | Bl Bool
    | Ctr String (List Value)


decodeValue : JD.Decoder Value
decodeValue =
    let
        decodeCtr =
            JD.map2 Ctr
                (JD.field "$" JD.string)
                (JD.succeed [])
    in
    JD.oneOf
        [ JD.map Num JD.float
        , JD.map Str JD.string
        , JD.map Bl JD.bool
        , decodeCtr
        ]
