module Main exposing (..)

import Browser
import Html exposing (Html, button, div, option, p, select, span, table, td, text, th, tr)
import Html.Attributes as Attributes exposing (selected)
import List exposing (map, concat)
import Models exposing (..)
import Input.Number exposing (..)
import Html.Events exposing (onClick, onInput)
import Styles exposing (calculateButtonStyle, calculateResultContainerStyle, parentContainerStyle, personInputContainerStyle, personInputTableStyle)

-- MAIN

main =
    Browser.sandbox { init = init, update = update, view = view}


-- MODEL

type alias PersonInputLine =
    { planet: Planet
    , degree: Int
    , sign: Sign
    , minute: Int }

type alias PersonInput = List PersonInputLine

type alias CalculationResultLine =
    { planet: Planet
    , aspect: Aspect }

type alias CalculationResult =
    { planet: Planet
    , results: List CalculationResultLine }

type alias Model =
    { personInput1: PersonInput
     , personInput2: PersonInput
     , result: Maybe (List CalculationResult) }

init : Model
init =
    { personInput1 = List.map (\e -> defaultPersonInputLine e) planets
    , personInput2 = List.map (\e -> defaultPersonInputLine e) planets
    , result = Nothing }

defaultPersonInputLine: Planet -> PersonInputLine
defaultPersonInputLine planet = PersonInputLine planet 0 Aquarius 0


-- UPDATE

type Msg =
    DegreeUpdate Person Planet Int
    | MinuteUpdate Person Planet Int
    | SignUpdate Person Planet Sign
    | Calculate
update : Msg -> Model -> Model
update msg model =
    case msg of
        DegreeUpdate person planet value ->
            updateForPerson model person (\a -> handleDegreeUpdate a planet value)
        MinuteUpdate person planet value ->
            updateForPerson model  person (\a -> handleMinuteUpdate a planet value)
        SignUpdate person planet sign ->
            updateForPerson model person (\a -> handleSignUpdate a planet sign)
        Calculate -> model

updateForPerson: Model -> Person -> (PersonInput -> PersonInput) -> Model
updateForPerson model person updateFunc =
    if(person == Person1) then
        { model | personInput1 = (updateFunc model.personInput1) }
    else
        { model | personInput2 = (updateFunc model.personInput2) }

updateForPlanet: PersonInput -> Planet -> (PersonInputLine -> PersonInputLine) -> PersonInput
updateForPlanet personInput planet updateFunc =
    List.map (\a -> if(a.planet == planet) then (updateFunc a) else a) personInput

handleDegreeUpdate: PersonInput -> Planet -> Int -> PersonInput
handleDegreeUpdate personInput planet value =
    updateForPlanet personInput planet (\a -> {a | degree = value})

handleMinuteUpdate: PersonInput -> Planet -> Int -> PersonInput
handleMinuteUpdate personInput planet value =
    updateForPlanet personInput planet (\a -> {a | minute = value})

handleSignUpdate: PersonInput -> Planet -> Sign -> PersonInput
handleSignUpdate personInput planet value =
    updateForPlanet personInput planet (\a -> {a | sign = value})

calculateResult: Model -> Model
calculateResult model = model



-- VIEW

handleIntInput: Maybe Int -> Int
handleIntInput int =
    case int of
        Nothing -> 0
        Just v -> v

handleSignInput: Maybe Sign -> Sign -> Sign
handleSignInput input fallback =
    case input of
        Just s -> s
        Nothing -> fallback

signSelect: PersonInputLine -> (Sign -> Msg) -> Html Msg
signSelect personInputLine onSelect =
    select [ onInput (\a -> onSelect (handleSignInput (signFromString a) personInputLine.sign)) ]
    (List.map (\a -> option [ Attributes.selected (a == personInputLine.sign) ] [text (signString a)]) signs)

numericInput: Int -> (Maybe Int -> Msg) -> Html Msg
numericInput value onChange =
    Input.Number.input
      { onInput = onChange
      , maxLength = Nothing
      , minValue = Just 0
      , maxValue = Just 360
      , hasFocus = Nothing} [] (Just value)


personInputLineView: PersonInputLine -> Person -> Html Msg
personInputLineView personInputLine person =
    tr []
    [ td [] [text (planetString personInputLine.planet)]
      , td [] [signSelect personInputLine (\a -> SignUpdate person personInputLine.planet a)]
      , td [] [numericInput personInputLine.degree (\a -> DegreeUpdate person personInputLine.planet (handleIntInput a))]
      , td [] [numericInput personInputLine.minute (\a -> MinuteUpdate person personInputLine.planet (handleIntInput a))]
    ]

personInputView: PersonInput -> Person -> Html Msg
personInputView personInput person =
    div []
        [ span[] [text (personString person)]
        , table []
          ([ tr[]
              [ th[] [text "Planet"]
              , th[] [text "Sign"]
              , th[] [text "Degree"]
              , th[] [text "Minute"]
              ]
          ] ++ List.map (\a -> personInputLineView a person) personInput)]

view: Model -> Html Msg
view model =
    div parentContainerStyle
    [ p [][ text "Synastry"]
    , div personInputContainerStyle
        [ div personInputTableStyle [(personInputView model.personInput1 Person1)]
        , div personInputTableStyle [(personInputView model.personInput2 Person2)]]
    , button (calculateButtonStyle ++ [onClick Calculate]) [text "Calculate"]
    , div calculateResultContainerStyle [text "Calculation result"]
    ]

