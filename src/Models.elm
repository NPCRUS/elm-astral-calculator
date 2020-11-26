module Models exposing (..)

-- DEFINITIONS

type Element = Fire | Earth | Air | Water

type Cross = Cardinal | Fixed | Mutable

type Planet = Sun | Moon | Mercury | Venus | Mars | Jupiter | Saturn | Pluto

type Sign = Aquarius | Pisces | Aries | Taurus | Gemini | Cancer | Leo | Virgo | Libra | Scorpio | Sagittarius | Capricorn

type Person = Person1 | Person2

type Aspect = Connection | Trigon | Quadrature | Opposition | Sextile | NoAspect

type alias ConstantRecord  =
    { sign: Sign
    , element: Element
    , cross: Cross }

calcConstants: List ConstantRecord
calcConstants =
    [ ConstantRecord Aries Fire Cardinal
    , ConstantRecord Taurus Earth Fixed
    , ConstantRecord Gemini Air Mutable
    , ConstantRecord Cancer Water Cardinal
    , ConstantRecord Leo Fire Fixed
    , ConstantRecord Virgo Earth Mutable
    , ConstantRecord Libra Air Cardinal
    , ConstantRecord Scorpio Water Fixed
    , ConstantRecord Sagittarius Fire Mutable
    , ConstantRecord Capricorn Earth Cardinal
    , ConstantRecord Aquarius Air Fixed
    , ConstantRecord Pisces Water Mutable]

-- HELPERS

planets: List Planet
planets = [ Sun, Moon, Mercury, Venus, Mars, Jupiter, Saturn, Pluto]

signs: List Sign
signs = [ Aquarius, Pisces, Aries, Taurus, Gemini, Cancer, Leo, Virgo, Libra, Scorpio, Sagittarius, Capricorn]

planetString: Planet -> String
planetString planet =
    case planet of
        Sun -> "Sun"
        Moon -> "Moon"
        Mercury -> "Mercury"
        Venus -> "Venus"
        Mars -> "Mars"
        Jupiter -> "Jupiter"
        Saturn -> "Saturn"
        Pluto -> "Pluto"

signString: Sign -> String
signString sign =
    case sign of
        Aquarius -> "Aquarius"
        Pisces -> "Pisces"
        Aries -> "Aries"
        Taurus -> "Taurus"
        Gemini -> "Gemini"
        Cancer -> "Cancer"
        Leo -> "Leo"
        Virgo -> "Virgo"
        Libra -> "Libra"
        Scorpio -> "Scorpio"
        Sagittarius -> "Sagittarius"
        Capricorn -> "Capricorn"

signFromString: String -> Maybe Sign
signFromString str =
    case str of
        "Aquarius" -> Just Aquarius
        "Pisces" -> Just Pisces
        "Aries" -> Just Aries
        "Taurus" -> Just Taurus
        "Gemini" -> Just Gemini
        "Cancer" -> Just Cancer
        "Leo" -> Just Leo
        "Virgo" -> Just Virgo
        "Libra" -> Just Libra
        "Scorpio" -> Just Scorpio
        "Sagittarius" -> Just Sagittarius
        "Capricorn" -> Just Capricorn
        _ -> Nothing

personString: Person -> String
personString person =
    case person of
        Person1 -> "Person1"
        Person2 -> "Person2"

aspectString: Aspect -> String
aspectString aspect =
    case aspect of
        Connection  -> "Connection"
        Trigon -> "Trigon"
        Quadrature -> "Quadrature"
        Opposition -> "Opposition"
        Sextile -> "Sextile"
        NoAspect -> "No Aspect"