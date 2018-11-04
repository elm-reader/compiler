module Traversal exposing (main)

import Browser
import Html exposing (div)
import List


main : Program () (List Int, List Int, Int) ()
main =
    Browser.sandbox
        { init = ( init, initWithoutPipes, fib10 )
        , update = update
        , view = view
        }


transform x =
    x * 11 - 12


filter x =
    (x |> modBy 2) == 0


init =
    List.range 0 10
        |> List.map (\x -> transform x)
        |> List.filter filter


initWithoutPipes =
    List.filter filter
        (List.map (\x -> transform x) (List.range 0 10))


fibonacci n =
    if n <= 1 then
        if equalZero n then
            n
        else
            n
    else
        fibonacci (n - 1) + fibonacci (n - 2)


equalZero n =
    case n of
        0 ->
            True

        _ ->
            False


fib10 =
    fibonacci 10


update a b =
    b


view x =
    div [] []
