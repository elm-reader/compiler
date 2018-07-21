module Reader
    exposing
        ( markInstrumented
        , parseConfig
        , recordCall
        , recordExpr
        , recordFrame
        , seq
        )

{-|

    Reader.

    @docs recordExpr, recordCall, recordFrame, markInstrumented, seq, parseConfig

-}

import Browser
import Debug exposing (toString)
import Elm.Kernel.Reader
import Html exposing (..)
import Html.Attributes exposing (property)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Reader.ProgramConfig as PC
import Reader.Tracing as Tracing


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
parseConfig : String -> Result JD.Error PC.Config
parseConfig =
    JD.decodeString PC.decodeConfig


main : Program () Model Msg
main =
    Elm.Kernel.Reader.main



-- MODEL


type alias Model =
    { programConfig : Result JD.Error PC.Config
    }



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update Msg ({ programConfig } as model) =
    ( model
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        cfg =
            case model.programConfig of
                Ok parsed ->
                    Debug.toString parsed

                Err errMsg ->
                    JD.errorToString errMsg
    in
    div []
        [ pre [] [ text cfg ]
        ]


viewOneLine : String -> Html msg
viewOneLine str =
    div [] [ text str ]
