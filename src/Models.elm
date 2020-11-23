module Models exposing (..)

-- DEFINITIONS

type Month = January | February | March | April | May | June | July | August | September | October | November | December

type Element = Fire | Earth | Air | Water

type Cross = Cardinal | Fixed | Mutable

type Planet = Sun | Moon | Mercury | Venus | Mars | Jupiter | Saturn | Pluto

type Sign = Aquarius | Pisces | Aries | Taurus | Gemini | Cancer | Leo | Virgo | Libra | Scorpio | Sagittarius | Capricorn

type Person = Person1 | Person2

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
        Aquarius -> "Aquaris"
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

personString: Person -> String
personString person =
    case person of
        Person1 -> "Person1"
        Person2 -> "Person2"