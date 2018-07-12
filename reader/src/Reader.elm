module Reader
    exposing
        ( markInstrumented
        , rawConfig
        , recordCall
        , recordExpr
        , recordFrame
        , seq
        )

{-|

    Reader.

    @docs recordExpr, recordCall, recordFrame, markInstrumented, seq, rawConfig

-}

import Browser
import Debug exposing (toString)
import Elm.Kernel.Reader
import Html exposing (..)
import Html.Attributes exposing (property)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Reader.ProgramConfig as PC


{-| -}
recordExpr : Int -> a -> a
recordExpr =
    Elm.Kernel.Reader.recordExpr


{-| -}
recordCall : Int -> (a -> b) -> (() -> c) -> c
recordCall =
    Elm.Kernel.Reader.recordCall


{-| -}
recordFrame : String -> (() -> a) -> a
recordFrame =
    Elm.Kernel.Reader.recordFrame


{-| -}
markInstrumented : (a -> b) -> (a -> b)
markInstrumented =
    Elm.Kernel.Reader.markInstrumented


{-| -}
seq : a -> b -> b
seq =
    Elm.Kernel.Reader.seq


{-| -}
rawConfig : String -> PC.Config
rawConfig =
    PC.Raw


main : Program () Model Msg
main =
    Elm.Kernel.Reader.main



-- MODEL


type alias Model =
    { count : Int
    , programConfig : PC.Config
    }


init : Model
init =
    { count = 0
    , programConfig = PC.Raw ""
    }



-- UPDATE


type Msg
    = RIncrement
    | RDecrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        RIncrement ->
            { model | count = increment model.count }

        RDecrement ->
            { model | count = decrement model.count }


decrement x =
    x - 2


increment : Int -> Int
increment x =
    x + 2


reset _ =
    init.count



-- VIEW


view : Model -> Html Msg
view model =
    let
        cfg =
            case model.programConfig of
                (PC.Parsed _ _) as parsed ->
                    Debug.toString parsed

                PC.Raw jsonText ->
                    case JD.decodeString PC.decodeConfig jsonText of
                        Ok parsed ->
                            Debug.toString parsed

                        Err e ->
                            JD.errorToString e
    in
    div []
        [ button [ onClick RDecrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick RIncrement ] [ text "+" ]
        , pre [] [ text cfg ]
        ]


viewOneLine : String -> Html msg
viewOneLine str =
    div [] [ text str ]
