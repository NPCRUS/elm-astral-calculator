module Translations exposing (..)

import Models exposing (Language(..))

type alias Translate = String -> String

trans: Language -> Translate
trans lang str =
    case lang of
        Eng -> str
        Ru -> translateToRussian str

translateToRussian: Translate
translateToRussian str =
    case str of
        "limit" -> "орбис"
        "Limit" -> "Орбис"
        "Person1" -> "Персона 1"
        "Person2" -> "Персона 2"
        "Planet" -> "Планета"
        "Sign" -> "Знак"
        "Degree" -> "Градусы"
        "Minute" -> "Минуты"
        "Sun" -> "Солнце"
        "Moon" -> "Луна"
        "Mercury" -> "Меркурий"
        "Venus" -> "Венера"
        "Mars" -> "Марс"
        "Jupiter" -> "Юпитер"
        "Saturn" -> "Сатурн"
        "Uranus" -> "Уран"
        "Neptune" -> "Нептун"
        "Pluto" -> "Плутон"
        "Aquarius" -> "Водолей"
        "Pisces" -> "Рыбы"
        "Aries" -> "Овен"
        "Taurus" -> "Телец"
        "Gemini" -> "Близнецы"
        "Cancer" -> "Рак"
        "Leo" -> "Лев"
        "Virgo" -> "Дева"
        "Libra" -> "Весы"
        "Scorpio" -> "Скорпион"
        "Sagittarius" -> "Стрелец"
        "Capricorn" -> "Козерог"
        "Calculate" -> "Расчитать"
        "Simple Synastry" -> "Простая синастрия"
        "Resonance" -> "Резонанс"
        "Physiology" -> "Физиология"
        "Gender" -> "Гендер"
        "Psychology" -> "Психология"
        "Conflict" -> "Конфликтность"
        "Perspective" -> "Перспектива"
        "Contact" -> "Контактность"
        "Press calculate to show result" -> "Нажми 'Расчитать', чтобы показать результат"
        "Connection" -> "Соединение"
        "Trigon" -> "Тригон"
        "Quadrature" -> "Квадратура"
        "Opposition" -> "Оппозиция"
        "Sextile" -> "Секстиль"
        "NoAspect" -> ""
        "Language" -> "Язык"
        "Eng" -> "Английский"
        "Ru" -> "Русский"
        _ -> "Не переведено!"