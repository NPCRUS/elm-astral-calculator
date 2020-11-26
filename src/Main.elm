module Main exposing (..)

import Browser
import Debug exposing (log, toString)
import Html exposing (Html, button, div, h2, option, p, select, span, table, td, text, th, tr)
import Html.Attributes as Attributes exposing (rowspan, selected)
import List exposing (map, concat)
import Models exposing (..)
import Input.Number exposing (..)
import Html.Events exposing (onClick, onInput)
import Styles exposing (calculateButtonStyle, calculateResultContainerStyle, parentContainerStyle, personInputContainerStyle, personInputTableStyle, tableElemStyle)

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

type alias PlanetCalculationResult =
    { planet: Planet
    , results: List CalculationResultLine }

type alias CalculationResult = Maybe (List PlanetCalculationResult)

type alias Model =
    { personInput1: PersonInput
     , personInput2: PersonInput
     , result: CalculationResult }

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
        Calculate -> updateForResult model calculateResult

updateForPerson: Model -> Person -> (PersonInput -> PersonInput) -> Model
updateForPerson model person updateFunc =
    if(person == Person1) then
        { model | personInput1 = (updateFunc model.personInput1) }
    else
        { model | personInput2 = (updateFunc model.personInput2) }

updateForPlanet: PersonInput -> Planet -> (PersonInputLine -> PersonInputLine) -> PersonInput
updateForPlanet personInput planet updateFunc =
    List.map (\a -> if(a.planet == planet) then (updateFunc a) else a) personInput

updateForResult: Model -> (Model -> CalculationResult) -> Model
updateForResult model updateFunc =
    { model | result = updateFunc model}

handleDegreeUpdate: PersonInput -> Planet -> Int -> PersonInput
handleDegreeUpdate personInput planet value =
    updateForPlanet personInput planet (\a -> {a | degree = value})

handleMinuteUpdate: PersonInput -> Planet -> Int -> PersonInput
handleMinuteUpdate personInput planet value =
    updateForPlanet personInput planet (\a -> {a | minute = value})

handleSignUpdate: PersonInput -> Planet -> Sign -> PersonInput
handleSignUpdate personInput planet value =
    updateForPlanet personInput planet (\a -> {a | sign = value})

calculateResult: Model -> CalculationResult
calculateResult model =
    Just ( List.map
            (\a ->
                PlanetCalculationResult
                a.planet
                (List.map (\b -> (calculateRelation a b)) model.personInput2)
            )
            model.personInput1)

calculateRelation: PersonInputLine -> PersonInputLine -> CalculationResultLine
calculateRelation personInput1 personInput2 =
    CalculationResultLine personInput2.planet Connection


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

complicatedSecondaryFilter: Planet -> CalculationResultLine -> Bool
complicatedSecondaryFilter parentPlanet calcLine =
    case parentPlanet of
        Moon -> List.member calcLine.planet [Sun, Venus, Mercury, Mars]
        Venus -> List.member calcLine.planet [Sun, Mars]
        Mercury -> List.member calcLine.planet [Sun]
        _ -> False

conflictSecondaryFilter: Planet -> CalculationResultLine -> Bool
conflictSecondaryFilter parentPlanet calcLine =
    case parentPlanet of
        Pluto -> List.member calcLine.planet [Saturn, Jupiter, Mars]
        Saturn -> List.member calcLine.planet [Pluto, Saturn, Jupiter, Mars]
        Jupiter-> List.member calcLine.planet [Pluto, Saturn, Jupiter, Mars]
        Mars -> List.member calcLine.planet [Pluto, Saturn, Jupiter, Mars]
        _ -> False

perspectiveSecondaryFilter: Planet -> CalculationResultLine -> Bool
perspectiveSecondaryFilter parentPlanet calcLine =
    case parentPlanet of
        Sun -> List.member calcLine.planet [Jupiter, Saturn]
        Moon -> List.member calcLine.planet [Jupiter, Saturn]
        Saturn -> List.member calcLine.planet [Sun, Moon]
        Jupiter -> List.member calcLine.planet [Sun, Moon]
        _ -> False

simpleSecondaryFilter: (List Planet) -> Planet -> CalculationResultLine -> Bool
simpleSecondaryFilter list _ calcLine =
    List.member calcLine.planet list


maybeResultView: CalculationResult -> Html Msg
maybeResultView result =
    case result of
        Nothing -> span [] [text "Press calculate to show result"]
        Just value -> div []
            [ h2 [] [text "Simple Synastry"]
            , resultView value "" [Moon, Venus, Mercury] complicatedSecondaryFilter
            , h2 [] [text "Resonance"]
            , resultView value "Physiology" [Sun, Mars] (simpleSecondaryFilter [Venus, Moon])
            , resultView value "Gender" [Venus, Moon] (simpleSecondaryFilter [Sun, Mars])
            , resultView value "Psychology" [Sun, Venus, Moon] (simpleSecondaryFilter [Sun, Venus, Moon])
            , resultView value "Conflict" [Pluto, Saturn, Jupiter, Mars] conflictSecondaryFilter
            , resultView value "Perspective" [Sun, Venus, Moon] perspectiveSecondaryFilter
            , resultView value "Contact" [Mercury] (simpleSecondaryFilter [Mercury])]

resultView: (List PlanetCalculationResult) -> String -> (List Planet) -> (Planet -> CalculationResultLine -> Bool) -> Html Msg
resultView list title mainPlanetFilter secondaryPlanetFilterFunc =
    div []
        [ span [] [text title]
        , table tableElemStyle
            (list
                |> List.filter (filterByMainPlanet mainPlanetFilter)
                |> List.map (\a ->
                                a.results
                                    |> List.filter (secondaryPlanetFilterFunc a.planet)
                                    |> (\c -> List.indexedMap (\i line -> resultViewLine a.planet line i (List.length c)) c)
                            )
                |> List.foldr utilityFold []
            )
        ]

filterByMainPlanet: (List Planet) -> PlanetCalculationResult -> Bool
filterByMainPlanet list planetCalculation = List.member planetCalculation.planet list

utilityFold: List (Html Msg) -> List(Html Msg) -> List(Html Msg)
utilityFold a b = List.concat [a, b]

resultViewLine: Planet -> CalculationResultLine -> Int -> Int -> Html Msg
resultViewLine planet calcLine index length =
    tr tableElemStyle
        (
            (if(index == 0) then
               [ th ([rowspan length] ++ tableElemStyle) [text (planetString planet)] ]
              else []) ++
            [ td tableElemStyle [text (planetString calcLine.planet)]
            , td tableElemStyle [text (aspectString calcLine.aspect)] ]
        )

view: Model -> Html Msg
view model =
    div parentContainerStyle
    [ p [][ text "Synastry"]
    , div personInputContainerStyle
        [ div personInputTableStyle [(personInputView model.personInput1 Person1)]
        , div personInputTableStyle [(personInputView model.personInput2 Person2)]]
    , button (calculateButtonStyle ++ [onClick Calculate]) [text "Calculate"]
    , div calculateResultContainerStyle [ maybeResultView model.result ]
    ]

