module Reader.ProgramConfig
    exposing
        ( Config(..)
        , ExprId
        , Frame
        , FrameId
        , Interface
        , Interfaces
        , SourceMap
        , SourceMaps
        , decodeConfig
        )

import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Tuple


type Config
    = Raw String
    | Parsed Interfaces SourceMaps


type alias Package =
    { author : String
    , project : String
    }


decodePackage : JD.Decoder Package
decodePackage =
    JD.map2 Package
        (JD.field "author" JD.string)
        (JD.field "project" JD.string)


type alias Module =
    { package : Package
    , mod : Name
    }


decodeModule : JD.Decoder Module
decodeModule =
    JD.map2 Module
        (JD.field "package" decodePackage)
        (JD.field "module" JD.string)


type alias Name =
    String


decodeConfig : JD.Decoder Config
decodeConfig =
    JD.map2 Parsed
        (JD.field "interfaces" decodeInterfaces)
        (JD.field "source_maps" decodeSourceMaps)



-- INTERFACES


type alias Interfaces =
    Dict Module Interface


decodeInterfaces =
    JD.succeed Dict.empty


type alias Interface =
    ()



-- SOURCE MAPS


type alias SourceMaps =
    Dict Module SourceMap


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
    { mod : Module
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
    , exprNames : Dict ExprId ( Module, Name )
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
