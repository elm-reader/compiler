module MultiFile2 exposing (main)

import Html exposing (Html, text)

-- Tricky glob import
import MultiFile1 exposing (..)


main : Html a
main =
    text foo
