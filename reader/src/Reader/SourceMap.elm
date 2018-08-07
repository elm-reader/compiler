module Reader.SourceMap
    exposing
        ( ExprId
        , Frame
        , FrameId
        , ModuleId
        , Position
        , Region
        , SourceMap
        , compareExprIds
        , compareFrameIds
        , compareModuleIds
        , comparePackageIds
        , comparePositions
        , decode
        , decodeExprId
        , decodeFrameId
        , decodeRegion
        , exprsEndingAt
        , exprsStartingAt
        , lookupRegionSource
        )

import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)


-- SOURCE MAPS


type alias SourceMap =
    { frames : Dict FrameId Frame
    , sources : Dict ModuleId String
    }


emptySourceMap =
    SourceMap Dict.empty Dict.empty


decode : JD.Decoder SourceMap
decode =
    JD.map2 SourceMap
        (JD.field "frames" <| Dict.decode compareFrameIds ( "id", decodeFrameId ) ( "frame", decodeFrame ))
        (JD.field "sources" <| Dict.decode compareModuleIds ( "module", decodeModuleId ) ( "source", JD.string ))



-- FRAME ID


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


compareFrameIds f1 f2 =
    case compareModuleIds f1.mod f2.mod of
        EQ ->
            case compare f1.def f2.def of
                EQ ->
                    compare f1.frameIndex f2.frameIndex

                other ->
                    other

        other ->
            other



-- FRAME


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
        (JD.field "expr_regions" <| Dict.decode compareExprIds ( "id", decodeExprId ) ( "regions", JD.list decodeRegion ))
        (JD.field "expr_names" <| Dict.decode compareExprIds ( "id", decodeExprId ) ( "qualified_name", decodeExprName ))



-- EXPR ID


type ExprId
    = ExprId Int


compareExprIds : ExprId -> ExprId -> Order
compareExprIds (ExprId i) (ExprId j) =
    compare i j


decodeExprId : JD.Decoder ExprId
decodeExprId =
    JD.map ExprId JD.int



-- PACKAGE ID


type PackageId
    = PackageId String -- in format "author/project"


comparePackageIds (PackageId p1) (PackageId p2) =
    compare p1 p2


decodePackageId : JD.Decoder PackageId
decodePackageId =
    JD.map PackageId JD.string



-- MODULE ID


type alias ModuleId =
    { package : PackageId
    , mod : Name
    }


compareModuleIds : ModuleId -> ModuleId -> Order
compareModuleIds m1 m2 =
    case comparePackageIds m1.package m2.package of
        EQ ->
            compare m1.mod m2.mod

        order ->
            order


decodeModuleId : JD.Decoder ModuleId
decodeModuleId =
    JD.map2 ModuleId
        (JD.field "package" decodePackageId)
        (JD.field "module" JD.string)


type alias Name =
    String



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



-- POSITION/REGION LOOKUP UTILS


isPositionInRegion : Position -> Region -> Bool
isPositionInRegion pos { start, end } =
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
        |> Dict.keyValuePairs
        |> List.filterMap
            (\( exprId, regions ) ->
                if List.any (isPositionInRegion pos) regions then
                    Just exprId
                else
                    Nothing
            )


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


{-| exprsStartingAt returns a list of the IDs of expressions with a region
starting at `pos`, in increasing order of the length of the associated region.
-}
exprsStartingAt : Position -> Dict ExprId (List Region) -> List ExprId
exprsStartingAt pos exprRegions =
    exprsWithARegionFulfilling (\region -> region.start == pos) exprRegions
        |> List.sortWith
            (\( _, r1 ) ( _, r2 ) -> comparePositions r1.end r2.end)
        |> List.map (\( exprId, _ ) -> exprId)


{-| exprsEndingAt returns a list of the IDs of expressions with a region
ending at `pos`, in decreasing order of the length of the associated region.
-}
exprsEndingAt : Position -> Dict ExprId (List Region) -> List ExprId
exprsEndingAt pos exprRegions =
    exprsWithARegionFulfilling (\region -> region.end == pos) exprRegions
        |> List.sortWith
            (\( _, r1 ) ( _, r2 ) -> comparePositions r1.start r2.start)
        |> List.map (\( exprId, _ ) -> exprId)


{-| Helper function. See exprsStartingAt and exprsEndingAt
-}
exprsWithARegionFulfilling : (Region -> Bool) -> Dict ExprId (List Region) -> List ( ExprId, Region )
exprsWithARegionFulfilling condition exprRegions =
    exprRegions
        |> Dict.keyValuePairs
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
