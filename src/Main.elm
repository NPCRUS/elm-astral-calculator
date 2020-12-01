module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as Attributes exposing (class, id, rowspan, value)
import List.Extra as List
import Models exposing (..)
import Input.Number as ExtraInput exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (toInt)
import Styles exposing (..)
import Translations exposing (Translate, trans)
import Tuple exposing (first, second)

-- MAIN

main =
    Browser.sandbox { init = init, update = update, view = view}


-- MODEL

type alias PersonInputLine =
    { planet: Planet
    , degree: Int
    , sign: Sign
    , minute: Int }

type alias PersonInput =
    { name: String
    , birthDate: String
    , birthPlace: String
    , values: List PersonInputLine }

type alias CalculationResultLine =
    { planet: Planet
    , aspect: Aspect }

type alias PlanetCalculationResult =
    { planet: Planet
    , results: List CalculationResultLine }

type alias CalculationResult =
    { simpleCalculationResult: List PlanetCalculationResult
    , complicatedCalculationResult: List PlanetCalculationResult }

type alias FullCalculationResult =
    { result: CalculationResult
    , reverseResult: CalculationResult }

type alias MaybeCalculationResult = Maybe FullCalculationResult

type alias Model =
    { personInput1: PersonInput
     , personInput2: PersonInput
     , limit: Int
     , language: Language
     , result: MaybeCalculationResult }

init : Model
init =
    { personInput1 = PersonInput "Person1" ""  ""(List.map (\e -> defaultPersonInputLine e) planets)
    , personInput2 = PersonInput "Person2" "" "" (List.map (\e -> defaultPersonInputLine e) planets)
    , limit = 6
    , language = Ru
    , result = Nothing }

defaultPersonInputLine: Planet -> PersonInputLine
defaultPersonInputLine planet = PersonInputLine planet 0 Aquarius 0


-- UPDATE

type Msg =
    DegreeUpdate Person Planet Int
    | MinuteUpdate Person Planet Int
    | SignUpdate Person Planet Sign
    | NameUpdate Person String
    | BirthDateUpdate Person String
    | BirthPlaceUpdate Person String
    | LimitUpdate Int
    | LanguageUpdate Language
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
        NameUpdate person value ->
            updateForPerson model person (\a -> {a | name = value})
        BirthDateUpdate person value ->
                    updateForPerson model person (\a -> {a | birthDate = value})
        BirthPlaceUpdate person value ->
                            updateForPerson model person (\a -> {a | birthPlace = value})
        LimitUpdate value -> {model | limit = value}
        LanguageUpdate value -> {model | language = value}
        Calculate -> updateForResult model calculateFullResult

updateForPerson: Model -> Person -> (PersonInput -> PersonInput) -> Model
updateForPerson model person updateFunc =
    if(person == Person1) then
        { model | personInput1 = (updateFunc model.personInput1) }
    else
        { model | personInput2 = (updateFunc model.personInput2) }

updateForPlanet: PersonInput -> Planet -> (PersonInputLine -> PersonInputLine) -> PersonInput
updateForPlanet personInput planet updateFunc =
    { personInput | values =
        List.map (\a -> if(a.planet == planet) then (updateFunc a) else a) personInput.values }

updateForResult: Model -> (Model -> MaybeCalculationResult) -> Model
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

calculateFullResult: Model -> MaybeCalculationResult
calculateFullResult model =
    Just (FullCalculationResult
        (calculateResult model.personInput1 model.personInput2 model.limit)
        (calculateResult model.personInput2 model.personInput1 model.limit))

calculateResult: PersonInput -> PersonInput -> Int -> CalculationResult
calculateResult personInput1 personInput2 limit =
    CalculationResult
        (calculateResultHelper personInput1 personInput2  limit False)
        (calculateResultHelper personInput1 personInput2  limit True)

calculateResultHelper: PersonInput -> PersonInput -> Int -> Bool -> (List PlanetCalculationResult)
calculateResultHelper personInput1 personInput2 limit withPrecision =
    List.map
        (\a ->
            PlanetCalculationResult
            a.planet
            (List.map (\b ->
                CalculationResultLine b.planet (calculateRelation a b (calculatePrecision withPrecision limit ))
            ) personInput2.values)
        )
        personInput1.values

calculateRelation: PersonInputLine -> PersonInputLine -> (PersonInputLine -> PersonInputLine -> Bool) -> Aspect
calculateRelation personInput1 personInput2 withPrecision =
    if((isConnection personInput1 personInput2) && (withPrecision personInput1 personInput2)) then
        Connection
    else if((isTrigon personInput1 personInput2) && (withPrecision personInput1 personInput2)) then
        Trigon
    else if((isOpposition personInput1 personInput2) && (withPrecision personInput1 personInput2)) then
        Opposition
    else if((isQuadrature personInput1 personInput2) && (withPrecision personInput1 personInput2)) then
        Quadrature
    else if((isSextile personInput1 personInput2) && (withPrecision personInput1 personInput2)) then
        Sextile
    else
        NoAspect

calculatePrecision: Bool -> Int -> PersonInputLine -> PersonInputLine -> Bool
calculatePrecision withPrecision limit personInput1 personInput2 =
    if(not withPrecision) then True
    else
        (toBaseDegree personInput1.degree - toBaseDegree personInput2.degree, personInput1.minute - personInput2.minute)
            |> (\t -> (first t) + (second t // abs (second t)))
            |> abs
            |> (>=) limit

toBaseDegree: Int -> Int
toBaseDegree n = n - (n // 30 * 30)

isConnection: PersonInputLine -> PersonInputLine -> Bool
isConnection personInput1 personInput2 =
    if(personInput1.sign == personInput2.sign) then True
    else False

isTrigon: PersonInputLine -> PersonInputLine -> Bool
isTrigon personInput1 personInput2 =
    calcConstants
        |> List.filter (\a -> a.sign == personInput1.sign || a.sign == personInput2.sign)
        |> List.map (\a -> toString a.element)
        |> List.allDifferent
        |> not

isQuadrature: PersonInputLine -> PersonInputLine -> Bool
isQuadrature personInput1 personInput2 =
    calcConstants
        |> List.filter (\a -> a.sign == personInput1.sign || a.sign == personInput2.sign)
        |> List.map (\a -> toString a.cross)
        |> List.allDifferent
        |> not

isOpposition: PersonInputLine -> PersonInputLine -> Bool
isOpposition personInput1 personInput2 =
    case (personInput1.sign, personInput2.sign) of
        (Pisces, Virgo) -> True
        (Virgo, Pisces) -> True
        (Aries, Libra) -> True
        (Libra, Aries) -> True
        (Taurus, Scorpio) -> True
        (Scorpio, Taurus) -> True
        (Gemini, Sagittarius) -> True
        (Sagittarius, Gemini) -> True
        (Capricorn, Cancer) -> True
        (Cancer, Capricorn) -> True
        (Leo, Aquarius) -> True
        (Aquarius, Leo) -> True
        (_, _) -> False

isSextile: PersonInputLine -> PersonInputLine -> Bool
isSextile personInput1 personInput2 =
    matchSextileSign personInput1.sign personInput2.sign

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

handleLanguageInput: Language -> Maybe Language -> Language
handleLanguageInput fallback input =
    case input of
        Just s -> s
        Nothing -> fallback

signSelect: PersonInputLine -> (Sign -> Msg) -> Translate -> Html Msg
signSelect personInputLine onSelect trans =
    select [ onInput (\a -> onSelect (handleSignInput (signFromString a) personInputLine.sign)) ]
    (List.map (\a -> option [ Attributes.selected (a == personInputLine.sign), Attributes.value (toString a) ]
        [text (a |> toString |> trans)]) signs)

numericInput: Int -> Int -> (Maybe Int -> Msg) -> Html Msg
numericInput value maxValue onChange =
    ExtraInput.input
      { onInput = onChange
      , maxLength = Nothing
      , minValue = Just 0
      , maxValue = Just maxValue
      , hasFocus = Nothing } [] (Just value)


personInputLineView: PersonInputLine -> Person -> Translate -> Html Msg
personInputLineView personInputLine person trans =
    tr []
    [ td [] [text (personInputLine.planet |> toString |> trans)]
      , td [] [signSelect personInputLine (\a -> SignUpdate person personInputLine.planet a) trans]
      , td [] [numericInput personInputLine.degree 30 (\a -> DegreeUpdate person personInputLine.planet (handleIntInput a))]
      , td [] [numericInput personInputLine.minute 60 (\a -> MinuteUpdate person personInputLine.planet (handleIntInput a))]
    ]

nameInput: String -> Translate -> (String -> Msg) -> Html Msg
nameInput name trans onChange =
    div []
        [ label [][text ((trans "Name") ++ ":")]
        , Html.input [ value name, onInput onChange] [] ]

birthDateInput: String -> Translate -> (String -> Msg) -> Html Msg
birthDateInput datetime trans onChange =
    div [marginTop "5px"]
        [ label [][text ((trans "Birthday") ++ ":")]
        , Html.input [ value datetime, onInput onChange] []]

birthPlaceInput: String -> Translate -> (String -> Msg) -> Html Msg
birthPlaceInput name trans onChange =
    div [marginTop "5px"]
        [ label [][text ((trans "Birthplace") ++ ":")]
        , Html.input [ value name, onInput onChange] [] ]

personInputView: PersonInput -> Person -> Translate -> List (Html Msg)
personInputView personInput person trans =
    [div []
            [ nameInput personInput.name trans (\a -> NameUpdate person a)
            , birthDateInput personInput.birthDate trans (\a -> BirthDateUpdate person a)
            , birthPlaceInput personInput.birthPlace trans (\a -> BirthPlaceUpdate person a) ]
        , table inputTableStyle
          ([ tr[]
              [ th[] [text (trans "Planet")]
              , th[] [text (trans "Sign")]
              , th[] [text (trans "Degree")]
              , th[] [text (trans "Minute")]
              ]
          ] ++ List.map (\a -> personInputLineView a person trans) personInput.values)]

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

personHeaderView: PersonInput -> PersonInput -> Html Msg
personHeaderView person1 person2 =
    div personHeaderViewStyle
        [ personDataView person1
        , personDataView person2 ]

personDataView: PersonInput -> Html Msg
personDataView person1 =
    div flexColumn
        [ h2 [] [text person1.name]
        , span [] [text person1.birthPlace]
        , span [] [text person1.birthDate]]

maybeResultView: Model -> Translate -> Html Msg
maybeResultView model trans =
    case model.result of
        Nothing -> span [] [text (trans "Press calculate to show result")]
        Just value ->
            div []
                [personHeaderView model.personInput1 model.personInput2
                , div flexSpaceAround
                    [ resultView value.result trans
                      , resultView value.reverseResult trans] ]

resultView: CalculationResult -> Translate -> Html Msg
resultView value  trans =
    div calculateResultContainerStyle
        [ h2 [] [text (trans "Simple Synastry")]
        , resultPartView value.simpleCalculationResult "" [Moon, Venus, Mercury] complicatedSecondaryFilter trans
        , h2 [] [text (trans "Resonance")]
        , resultPartView value.complicatedCalculationResult (trans "Physiology") [Sun, Mars] (simpleSecondaryFilter [Venus, Moon]) trans
        , resultPartView value.complicatedCalculationResult (trans "Gender") [Venus, Moon] (simpleSecondaryFilter [Sun, Mars]) trans
        , resultPartView value.complicatedCalculationResult (trans "Psychology") [Sun, Venus, Moon] (simpleSecondaryFilter [Sun, Venus, Moon]) trans
        , div [class "only-print", marginTop "40px"] []
        , resultPartView value.complicatedCalculationResult (trans "Conflict") [Pluto, Saturn, Jupiter, Mars] conflictSecondaryFilter trans
        , resultPartView value.complicatedCalculationResult (trans "Perspective") [Sun, Venus, Moon] perspectiveSecondaryFilter trans
        , resultPartView value.complicatedCalculationResult (trans "Contact") [Mercury] (simpleSecondaryFilter [Mercury]) trans]

resultPartView: (List PlanetCalculationResult) -> String -> (List Planet) -> (Planet -> CalculationResultLine -> Bool) -> Translate -> Html Msg
resultPartView list title mainPlanetFilter secondaryPlanetFilterFunc trans =
    div []
        [ h4 [] [text title]
        , table tableElemStyle
            (list
                |> List.filter (filterByMainPlanet mainPlanetFilter)
                |> List.map (\a ->
                                a.results
                                    |> List.filter (secondaryPlanetFilterFunc a.planet)
                                    |> (\c -> List.indexedMap (\i line -> resultViewLine a.planet line i (List.length c) trans) c)
                            )
                |> List.foldr utilityFold []
            )
        ]

filterByMainPlanet: (List Planet) -> PlanetCalculationResult -> Bool
filterByMainPlanet list planetCalculation = List.member planetCalculation.planet list

utilityFold: List (Html Msg) -> List(Html Msg) -> List(Html Msg)
utilityFold a b = List.concat [a, b]

resultViewLine: Planet -> CalculationResultLine -> Int -> Int -> Translate -> Html Msg
resultViewLine planet calcLine index length trans =
    tr tableElemStyle
        (
            (if(index == 0) then
               [ th ([rowspan length] ++ tableSecondaryStyle) [text (planet |> toString |> trans)] ]
              else []) ++
            [ td tableSecondaryStyle [text (calcLine.planet |> toString |> trans)]
            , td tableSecondaryStyle [text (calcLine.aspect |> toString |> trans)] ]
        )

toolbarView: Model -> Translate -> Html Msg
toolbarView model trans =
    div toolbarContainerStyle
        [ limitSelectionView model.limit trans
        , languageSelectionView model.language trans]

limitSelectionView: Int -> Translate -> Html Msg
limitSelectionView limit trans =
    div ((marginLeft "5px") :: toolbarRowStyle) [
        label [] [text ((trans "Limit") ++ ":")]
        , select [ onInput (\a -> LimitUpdate (handleIntInput (toInt a))) ]
            (List.map (\a -> option [Attributes.selected (a == limit), Attributes.value (toString a)]
                [text (toString a)]) limitDistribution) ]

languageSelectionView: Language -> Translate -> Html Msg
languageSelectionView language trans =
    div toolbarRowStyle [
        label [] [text ((trans "Language") ++ ":")]
        , select [ onInput (\a -> a |> languageFromString |> handleLanguageInput language |> LanguageUpdate)]
            (List.map (\a -> option [Attributes.selected (a == language), Attributes.value (toString a)]
                [text (a |> toString |> trans)]) languages)]

view: Model -> Html Msg
view model =
    let
        translateFunc = trans model.language
    in
    div parentContainerStyle
    [ div (noPrintContainerStyle ++ [id "no-print"]) [toolbarView model translateFunc
    , div personInputContainerStyle
        [ div personInputTableStyle (personInputView model.personInput1 Person1 translateFunc)
        , div personInputTableStyle (personInputView model.personInput2 Person2 translateFunc)]
    , button (calculateButtonStyle ++ [onClick Calculate]) [text (translateFunc "Calculate")]]
    , div []
        [maybeResultView model translateFunc]
    ]

