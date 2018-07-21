module Counter exposing (main, mainish)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)


-- WHOMST


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


ex1 : Int
ex1 =
    12312


ex2 : Int
ex2 =
    4324


mainish : Int
mainish =
    -- calc GCD of ex1 and ex2
    euclid ex1 ex2


euclid : Int -> Int -> Int
euclid a b =
    if b == 0 then
        a
    else
        euclid b (a |> modBy b)



-- MODEL


type alias Model =
    { count : Whomstery
    }


type Whomstery
    = Whomsted
    | RealNews Int


toNum whomstery =
    case whomstery of
        Whomsted ->
            mainish

        RealNews x ->
            x


init : Model
init =
    { count = Whomsted
    }



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = increment model.count }

        Decrement ->
            { model | count = decrement model.count }


decrement : Whomstery -> Whomstery
decrement x =
    RealNews <| toNum x - 1


increment : Whomstery -> Whomstery
increment x =
    RealNews <| toNum x + 1


reset _ =
    Whomsted



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt <| toNum model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
