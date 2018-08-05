module Reader.TraceData
    exposing
        ( Expr
        , Frame(..)
        , InstrumentedFrameData
        , TraceData(..)
        , childFrames
        , decode
        , valueToString
        )

import Debug
import Json.Decode as JD
import Json.Encode as JE
import Reader.Dict as D
import Reader.SourceMap as SourceMap exposing (SourceMap)
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
    , exprs : D.Dict SourceMap.ExprId Expr
    }


decodeFrameTrace : JD.Decoder Frame
decodeFrameTrace =
    let
        decFrame =
            JD.lazy (\() -> decodeFrameTrace)

        decExpr =
            JD.lazy (\() -> decodeExpr)

        decodeInstrumented =
            JD.map2 (\id exprs -> InstrumentedFrame { id = id, exprs = exprs })
                (JD.field "id" SourceMap.decodeFrameId)
                (JD.field "exprs" <| D.decode SourceMap.compareExprIds ( "id", SourceMap.decodeExprId ) ( "expr", decExpr ))

        decodeNonInstrumented =
            JD.map NonInstrumentedFrame
                (JD.field "child_frames" <| JD.list decFrame)
    in
    JD.oneOf [ decodeNonInstrumented, decodeInstrumented ]


childFrames : Frame -> List Frame
childFrames frameTrace =
    case frameTrace of
        InstrumentedFrame { exprs } ->
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



-- EXPRESSION TRACES


{-| -}
type alias Expr =
    { value : Maybe Value
    , childFrame : Maybe Frame -- frame that created the expression
    }


decodeExpr : JD.Decoder Expr
decodeExpr =
    JD.map2 Expr
        (JD.maybe <| JD.field "val" decodeValue)
        (JD.field "child_frame" <| JD.nullable decodeFrameTrace)



-- VALUE


{-| Value represents an arbitrary Elm value, from a trace.
-}
type Value
    = Num Float
    | Str String
    | Bl Bool
    | Ctr String (List Value)
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
