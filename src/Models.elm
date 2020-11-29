module Models exposing (..)

-- DEFINITIONS

type Element = Fire | Earth | Air | Water

type Cross = Cardinal | Fixed | Mutable

type Planet = Sun | Moon | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune | Pluto

type Sign = Aquarius | Pisces | Aries | Taurus | Gemini | Cancer | Leo | Virgo | Libra | Scorpio | Sagittarius | Capricorn

type Person = Person1 | Person2

type Aspect = Connection | Trigon | Quadrature | Opposition | Sextile | NoAspect

type Language = Eng | Ru

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

limitDistribution: List Int
limitDistribution = [5, 6, 7]

-- HELPERS

planets: List Planet
planets = [Sun, Moon, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto]

signs: List Sign
signs = [Aquarius, Pisces, Aries, Taurus, Gemini, Cancer, Leo, Virgo, Libra, Scorpio, Sagittarius, Capricorn]

languages: List Language
languages = [Eng, Ru]

matchSextileSign: Sign -> Sign -> Bool
matchSextileSign sign1 sign2 =
    case (sign1, sign2) of
        (Pisces, Capricorn) -> True
        (Pisces, Taurus) -> True
        (Aries, Aquarius) -> True
        (Aries, Gemini) -> True
        (Taurus, Pisces) -> True
        (Taurus, Cancer) -> True
        (Gemini, Aries) -> True
        (Gemini, Leo) -> True
        (Cancer, Taurus) -> True
        (Cancer, Virgo) -> True
        (Leo, Gemini) -> True
        (Leo, Libra) -> True
        (Virgo, Cancer) -> True
        (Virgo, Scorpio) -> True
        (Libra, Leo) -> True
        (Libra, Sagittarius) -> True
        (Scorpio, Virgo) -> True
        (Scorpio, Capricorn) -> True
        (Sagittarius, Libra) -> True
        (Sagittarius, Aquarius) -> True
        (Capricorn, Scorpio) -> True
        (Capricorn, Pisces) -> True
        (Aquarius, Sagittarius) -> True
        (Aquarius, Aries) -> True
        (_, _) -> False

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

languageFromString: String -> Maybe Language
languageFromString str =
    case str of
        "Eng" -> Just Eng
        "Ru" -> Just Ru
        _ -> Nothing