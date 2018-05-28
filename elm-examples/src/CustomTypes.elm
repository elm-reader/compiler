module CustomTypes exposing (main)

import Html exposing (Html, text)


type Foo = Foo Int


type Bar = Bar1 Int | Bar2 String


type alias Baz = { x : Int, y : String }


myBaz : Baz
myBaz =
    Baz 42 "hello"


main : Html a
main =
    text myBaz.y
