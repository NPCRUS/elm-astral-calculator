module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

personInputTableStyle: List (Attribute msg)
personInputTableStyle =
    [ style "display" "flex"
      , style "flex-direction" "column"]

personInputContainerStyle: List (Attribute msg)
personInputContainerStyle =
    [ style "display" "flex"
    , style "justify-content" "space-around"]

parentContainerStyle: List (Attribute msg)
parentContainerStyle =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "width" "800px"]

calculateButtonStyle: List (Attribute msg)
calculateButtonStyle =
    [ style "margin-top" "10px"
    , style "width" "200px"
    , style "align-self" "center"]

calculateResultContainerStyle: List (Attribute msg)
calculateResultContainerStyle =
    [ style "display" "flex"
    , style "margin-top" " 10px"]

