module Traversal exposing (main)

import Browser
import Html exposing (div)
import List


main =
    Browser.sandbox
        { init = init
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


update a b =
    b


view x =
    div [] []
