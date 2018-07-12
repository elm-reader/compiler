module Zounter exposing (main)

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



-- MODEL


type alias Model =
    { count : Int
    }


init : Model
init =
    { count = 0
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


decrement x =
    x - 1


increment : Int -> Int
increment x =
    x + 1


reset _ =
    init.count



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
