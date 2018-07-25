module Reader.ProgramConfig
    exposing
        ( Config
        , ExprId
        , Frame
        , FrameId
        , Interface
        , Interfaces
        , Region
        , SourceMap
        , TraceData(..)
        , TraceFrame(..)
        , childFrames
        , decodeConfig
        , decodeTraceData
        , emptyInterfaces
        , emptySourceMap
        , lookupRegionSource
        )

import Debug
import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Tuple


-- PROGRAM CONFIG


type alias Config =
    { interfaces : Interfaces, sourceMap : SourceMap, traceData : TraceData }


decodeConfig : JD.Decoder Config
decodeConfig =
    JD.map3 Config
        (JD.field "interfaces" decodeInterfaces)
        (JD.field "source_map" decodeSourceMap)
        (JD.field "traces" decodeTraceData)


type PackageId
    = PackageId String -- in format "author/project"


decodePackageId : JD.Decoder PackageId
decodePackageId =
    JD.map PackageId JD.string


type alias ModuleId =
    { package : PackageId
    , mod : Name
    }


decodeModuleId : JD.Decoder ModuleId
decodeModuleId =
    JD.map2 ModuleId
        (JD.field "package" decodePackageId)
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


type alias SourceMap =
    { frames : Dict FrameId Frame
    , sources : Dict ModuleId String
    }


lookupRegionSource : Region -> Dict ModuleId String -> Maybe String
lookupRegionSource { mod, start, end } sources =
    let
        -- nthLine returns the character position at which the nth line starts
        nthLine n str =
            if str == "" then
                Nothing
            else if n <= 1 then
                Just 0
            else
                let
                    newlines =
                        String.indices "\n" str

                    lineBreakPos =
                        List.head <| List.drop (n - 2) newlines
                in
                Maybe.map ((+) 1) lineBreakPos

        modSource =
            Dict.lookup mod sources

        startPos =
            modSource
                |> Maybe.andThen (nthLine start.line)
                |> Maybe.map ((+) (start.col - 1))

        endPos =
            modSource
                |> Maybe.andThen (nthLine end.line)
                |> Maybe.map ((+) (end.col - 1))
    in
    case ( startPos, endPos ) of
        ( Just startIndex, Just endIndex ) ->
            modSource
                |> Maybe.map (String.slice startIndex endIndex)

        ( _, _ ) ->
            Nothing


emptySourceMap =
    SourceMap Dict.empty Dict.empty


decodeSourceMap : JD.Decoder SourceMap
decodeSourceMap =
    JD.map2 SourceMap
        (JD.field "frames" <| decodeDict ( "id", decodeFrameId ) ( "frame", decodeFrame ))
        (JD.field "sources" <| decodeDict ( "module", decodeModuleId ) ( "source", JD.string ))


type alias FrameId =
    { mod : ModuleId
    , def : Name
    , frameIndex : Int
    }


decodeFrameId : JD.Decoder FrameId
decodeFrameId =
    JD.map3 FrameId
        (JD.field "module" decodeModuleId)
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
                (JD.field "module" decodeModuleId)
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
    { mod : ModuleId
    , start : Position
    , end : Position
    }


decodeRegion : JD.Decoder Region
decodeRegion =
    JD.map3 Region
        (JD.field "module" decodeModuleId)
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


childFrames : TraceFrame -> List TraceFrame
childFrames traceFrame =
    case traceFrame of
        InstrumentedFrame _ exprs ->
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
