module Utils.StringNumberConversion exposing (..)

import Regex
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)


myLocale : Locale
myLocale =
    { decimals = 2
    , thousandSeparator = "."
    , decimalSeparator = ","
    }


stringToFloatBrazilFormat : String -> Maybe Float
stringToFloatBrazilFormat string =
    let
        stringWithoutThousandSeparators =
            Regex.replace Regex.All (Regex.regex "[.]") (\_ -> "") string

        stringWithDecimalSeparatorsInAmericanFormat =
            Regex.replace Regex.All (Regex.regex ",") (\_ -> ".") stringWithoutThousandSeparators
    in
        case Result.toMaybe (String.toFloat stringWithDecimalSeparatorsInAmericanFormat) of
            Nothing ->
                Nothing

            Just float ->
                Just float


floatToStringBrazilFormat : Float -> String
floatToStringBrazilFormat float =
    format myLocale float
