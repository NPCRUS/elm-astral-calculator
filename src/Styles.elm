module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

personInputTableStyle: List (Attribute msg)
personInputTableStyle =
    [ style "display" "flex"
      , style "flex-direction" "column"
      , style "width" "400px"]

personInputContainerStyle: List (Attribute msg)
personInputContainerStyle =
    [ style "display" "flex" ]


