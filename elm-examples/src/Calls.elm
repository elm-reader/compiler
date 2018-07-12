module Calls exposing (call1, call2, constructorCall, explicitlyTyped, implicitlyTyped, listConstructorCall)

import Html exposing (Html, text)


explicitlyTyped : Int -> Int -> Int
explicitlyTyped x y =
    x * y


implicitlyTyped x y =
    x * y


call1 =
    explicitlyTyped 2 3


call2 =
    implicitlyTyped 2 3


constructorCall =
    Just 6


listConstructorCall =
    5 :: []
