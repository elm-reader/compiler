module Reader.Flex
    exposing
        ( column
        , columnWith
        , row
        , rowWith
        )

import Html exposing (Html)
import Html.Attributes as A


row : List (Html msg) -> Html msg
row =
    rowWith []


rowWith : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rowWith attrs contents =
    Html.div
        ([ displayFlex
         , A.style "flex-direction" "row"
         ]
            ++ attrs
        )
        contents


column : List (Html msg) -> Html msg
column =
    columnWith []


columnWith : List (Html.Attribute msg) -> List (Html msg) -> Html msg
columnWith attrs contents =
    Html.div
        ([ displayFlex
         , A.style "flex-direction" "column"
         ]
            ++ attrs
        )
        contents


displayFlex =
    A.style "display" "flex"
