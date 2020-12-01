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
    , style "margin" "5px 0"]

noPrintContainerStyle: List (Attribute msg)
noPrintContainerStyle =
    [ style "display" "flex"
    , style "flex-direction" "column"]

calculateButtonStyle: List (Attribute msg)
calculateButtonStyle =
    [ style "margin-top" "10px"
    , style "width" "200px"
    , style "align-self" "center"]

calculateResultContainerStyle: List (Attribute msg)
calculateResultContainerStyle =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "margin-top" " 10px"]

tableElemStyle: List (Attribute msg)
tableElemStyle =
    [ style "border" "1px solid black"
    , style "border-collapse" "collapse"]

tableSecondaryStyle: List (Attribute msg)
tableSecondaryStyle =
    [ style "border" "1px solid black"
    , style "width" "100px"]

toolbarContainerStyle: List (Attribute msg)
toolbarContainerStyle =
    [ style "border-bottom" "1px solid black"
    , style "display" "flex"
    , style "height" "25px"
    , style "margin-bottom" "10px"]

toolbarRowStyle: List (Attribute msg)
toolbarRowStyle =
    [ style "margin-right" "10px"]

displayFlex: Attribute msg
displayFlex =
    style "display" "flex"

flexColumn: List (Attribute msg)
flexColumn =
    [ displayFlex
    , style "flex-direction" "column" ]

flexSpaceAround: List (Attribute msg)
flexSpaceAround =
    [ displayFlex
    , style "justify-content" "space-around"]

marginTop: String -> Attribute msg
marginTop margin =
    style "margin-top" margin

alignItems: String -> Attribute msg
alignItems align =
    style "align-items" align

personHeaderViewStyle: List (Attribute msg)
personHeaderViewStyle =
    (marginTop "20px") :: (alignItems "center") :: flexSpaceAround

inputTableStyle: List (Attribute msg)
inputTableStyle =
    [ style "width" "330px"
    , style "align-self" "center"]

marginLeft: String -> Attribute msg
marginLeft margin =
    style "margin-left" margin

