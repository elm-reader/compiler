module Reader.ProgramConfig
    exposing
        ( Config
        , ExprId
        , Frame
        , FrameId
        , Interface
        , Interfaces
        , Position
        , Region
        , SourceMap
        , TraceData(..)
        , TraceExpr
        , TraceFrame(..)
        , childFrames
        , compareExprIds
        , comparePositions
        , decodeConfig
        , decodeTraceData
        , emptyInterfaces
        , emptySourceMap
        , exprsEndingHere
        , exprsStartingHere
        , lookupRegionSource
        , valueToString
        )

import Debug
import Json.Decode as JD
import Json.Encode as JE
import Reader.Dict as D
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
    D.Dict ModuleId Interface


emptyInterfaces =
    D.empty


decodeInterfaces =
    JD.succeed D.empty


type alias Interface =
    ()



-- SOURCE MAPS


type alias SourceMap =
    { frames : D.Dict FrameId Frame
    , sources : D.Dict ModuleId String
    }


lookupRegionSource : Region -> D.Dict ModuleId String -> Maybe String
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
            D.lookup mod sources

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


positionInRegion : Position -> Region -> Bool
positionInRegion pos { start, end } =
    let
        between left right x =
            x > left && x < right
    in
    (pos.line |> between start.line end.line)
        || (pos.line == start.line && pos.col >= start.col)
        || (pos.line == end.line && pos.col < end.col)


lookupPositionExprIds : Frame -> Position -> List ExprId
lookupPositionExprIds { exprRegions } pos =
    exprRegions
        |> D.keyValuePairs
        |> List.filterMap
            (\( exprId, regions ) ->
                if List.any (positionInRegion pos) regions then
                    Just exprId
                else
                    Nothing
            )


exprsWithARegionFulfilling : (Region -> Bool) -> D.Dict ExprId (List Region) -> List ( ExprId, Region )
exprsWithARegionFulfilling condition exprRegions =
    exprRegions
        |> D.keyValuePairs
        |> List.filterMap
            (\( exprId, regions ) ->
                case List.filter condition regions of
                    [ region ] ->
                        Just ( exprId, region )

                    firstRegion :: _ ->
                        Debug.log
                            ("exprsWithARegionFulfilling: got multiple overlapping regions for expr " ++ Debug.toString exprId)
                            (Just ( exprId, firstRegion ))

                    [] ->
                        Nothing
            )


{-| exprsStartingHere returns a list of the IDs of expressions with a region
starting at `pos`, in increasing order of the length of the associated region.
-}
exprsStartingHere : Position -> D.Dict ExprId (List Region) -> List ExprId
exprsStartingHere pos exprRegions =
    exprsWithARegionFulfilling (\region -> region.start == pos) exprRegions
        |> List.sortWith
            (\( _, r1 ) ( _, r2 ) -> comparePositions r1.end r2.end)
        |> List.map (\( exprId, _ ) -> exprId)


{-| exprsEndingHere returns a list of the IDs of expressions with a region
ending at `pos`, in decreasing order of the length of the associated region.
-}
exprsEndingHere : Position -> D.Dict ExprId (List Region) -> List ExprId
exprsEndingHere pos exprRegions =
    exprsWithARegionFulfilling (\region -> region.end == pos) exprRegions
        |> List.sortWith
            (\( _, r1 ) ( _, r2 ) -> comparePositions r1.start r2.start)
        |> List.map (\( exprId, _ ) -> exprId)


emptySourceMap =
    SourceMap D.empty D.empty


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
    , exprRegions : D.Dict ExprId (List Region)
    , exprNames : D.Dict ExprId ( ModuleId, Name )
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


compareExprIds : ExprId -> ExprId -> Order
compareExprIds (ExprId i) (ExprId j) =
    compare i j


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


comparePositions : Position -> Position -> Order
comparePositions p1 p2 =
    if p1.line == p2.line then
        compare p1.col p2.col
    else
        compare p1.line p2.line



-- GENERAL DECODE UTILS


decodeDict : ( String, JD.Decoder key ) -> ( String, JD.Decoder value ) -> JD.Decoder (D.Dict key value)
decodeDict ( keyName, decodeKey ) ( valName, decodeVal ) =
    let
        decodeEntry =
            JD.map2 Tuple.pair
                (JD.field keyName decodeKey)
                (JD.field valName decodeVal)
    in
    JD.map D.fromList (JD.list decodeEntry)



-- TRACE DATA


type TraceData
    = TraceData (List TraceFrame)


decodeTraceData : JD.Decoder TraceData
decodeTraceData =
    JD.map TraceData <| JD.list decodeTraceFrame


{-| -}
type TraceFrame
    = InstrumentedFrame FrameId (D.Dict ExprId TraceExpr)
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
            D.values exprs
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
    | Ctr String (List Value) -- FIXME: remove (String, -)
    | Function
    | Raw JD.Value


isCtrBinop : String -> Bool
isCtrBinop name =
    -- If it doesn't contain any letters, it's a binop.
    -- TODO: this can be replaced with something precise by looking through the compiler
    String.toLower name == String.toUpper name


isCtrTuple : String -> Bool
isCtrTuple name =
    String.startsWith "#" name


valueToString : Value -> String
valueToString val =
    let
        generalCtrToString ctr args =
            ctr ++ " " ++ String.join " " (List.map valueToStringEnclosed args)
    in
    case val of
        Num f ->
            String.fromFloat f

        Str s ->
            s

        Bl True ->
            "True"

        Bl False ->
            "False"

        Ctr ctr args ->
            if isCtrTuple ctr then
                -- Tuples have the constructor as "#[n]" where [n] is the number of elements
                "(" ++ String.join ", " (List.map valueToString args) ++ ")"
            else
                case args of
                    [] ->
                        ctr

                    [ a, b ] ->
                        if isCtrBinop ctr then
                            String.join " " [ valueToString a, ctr, valueToString b ]
                        else
                            generalCtrToString ctr args

                    _ ->
                        generalCtrToString ctr args

        Function ->
            -- TODO: specify a pattern for special non-printable values like this
            "#<function>"

        Raw json ->
            JE.encode 4 json


valueToStringEnclosed : Value -> String
valueToStringEnclosed val =
    let
        shouldEnclose =
            -- enclose the value in parens if it is a constructor call, with params, and isn't a tuple
            case val of
                Ctr ctr (_ :: _) ->
                    not (isCtrTuple ctr)

                _ ->
                    False
    in
    if shouldEnclose then
        "(" ++ valueToString val ++ ")"
    else
        valueToString val


decodeValue : JD.Decoder Value
decodeValue =
    let
        lazyDecodeValue =
            JD.lazy (\() -> decodeValue)

        decodeCtr =
            let
                decodeArgs =
                    JD.keyValuePairs lazyDecodeValue
                        |> JD.map (List.filter (\( k, _ ) -> k /= "$"))
                        -- TODO: sort properly. For more than 26 params, it will order them as 'a, b, ..., z, A, B, ...'
                        -- sort by the keys, which are in alphabetical order.
                        |> JD.map (List.sortBy Tuple.first)
                        |> JD.map (List.map Tuple.second)
            in
            JD.map2 Ctr
                (JD.field "$" JD.string)
                decodeArgs

        decodeFunc =
            JD.map (\_ -> Function)
                (JD.field "#<function>" (JD.succeed ()))

        decodeList =
            JD.map3
    in
    JD.oneOf
        [ JD.map Num JD.float
        , JD.map Str JD.string
        , JD.map Bl JD.bool
        , decodeCtr
        , decodeFunc
        ]
