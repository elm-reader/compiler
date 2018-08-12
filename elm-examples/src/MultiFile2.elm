module MultiFile2 exposing (main)

-- Tricky glob import

import Html exposing (Html, text)
import MultiFile1 exposing (..)


main : Html a
main =
    text (foo ++ String.fromInt fact10 ++ String.fromInt fib10)


factorial n =
    case n of
        0 ->
            1

        _ ->
            n * factorial (n - 1)


fact10 =
    factorial 10
