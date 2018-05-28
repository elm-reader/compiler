module Let exposing (main)

import Html exposing (Html, text)

main : Html a
main =
    let myVar = "hello"
    in text myVar
