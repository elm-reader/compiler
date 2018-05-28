module Case exposing (main)

import Html exposing (Html, text)


myMaybe =
    Just "hello"


main : Html a
main =
    case myMaybe of
        Just str ->
            text str

        Nothing ->
            text "goodbye"
