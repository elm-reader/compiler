module Constructors exposing (main)

import Browser
import Html exposing (div)
import List


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type MyUnion
    = First
    | Second Int
    | Third Int MyUnion


type MyBigStruct
    = One Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int


waste : a -> ()
waste _ =
    ()


generateThings : () -> List ()
generateThings () =
    [ waste First
    , waste (Second 10)
    , waste (Third 5 (Second 12))
    , waste []
    , waste [ 1, 2, 3, 4 ]
    , waste (1 :: 2 :: 3 :: 4 :: [])
    , waste [ First ]
    , waste (First :: Second 2 :: Third 3 First :: [])
    , waste Nothing
    , waste (Just 10)
    , waste (One 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28)
    , waste (Just ( 1, 2, 3 ))
    , waste (Just ())
    ]


init =
    generateThings ()


update a b =
    b


view x =
    div [] []
