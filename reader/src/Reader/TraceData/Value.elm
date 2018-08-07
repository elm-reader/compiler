module Reader.TraceData.Value
    exposing
        ( Value(..)
        , decode
        , isCtrBinop
        , isCtrTuple
        , isEqual
        , toString
        )

import Json.Decode as JD
import Json.Encode as JE


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


isEqual : Value -> Value -> Bool
isEqual v u =
    if v == Function || u == Function then
        False
    else
        v == u


toString : Value -> String
toString val =
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
                "(" ++ String.join ", " (List.map toString args) ++ ")"
            else
                case args of
                    [] ->
                        ctr

                    [ a, b ] ->
                        if isCtrBinop ctr then
                            String.join " " [ toString a, ctr, toString b ]
                        else
                            generalCtrToString ctr args

                    _ ->
                        generalCtrToString ctr args

        Function ->
            "<function>"

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
        "(" ++ toString val ++ ")"
    else
        toString val


decode : JD.Decoder Value
decode =
    let
        lazyDecodeValue =
            JD.lazy (\() -> decode)

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
