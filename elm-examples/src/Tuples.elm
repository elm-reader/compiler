module Tuples exposing (main)

import Html exposing (Html, text)


myTuple =
    ("hello", "world", 42)


main =
    let (message, _, _) = myTuple
    in text message
