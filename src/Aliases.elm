module Aliases exposing (..)

import Regex


type alias Unit =
    { id : String
    , name : String
    , initials : String
    }


initUnit : Unit
initUnit =
    { id = ""
    , name = ""
    , initials = ""
    }


type alias UnitDB =
    { id : String
    , name : String
    , initials : String
    }


getUnitByIdFromList : List Unit -> String -> Maybe Unit
getUnitByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getUnitByIdFromList xs id


type alias Message =
    { messageClass : String
    , header : String
    , text : String
    , active : Bool
    }


initMessage : Message
initMessage =
    { messageClass = ""
    , header = ""
    , text = ""
    , active = False
    }


type alias FieldError =
    { fieldName : String
    , errorMessage : Maybe String
    }


errorMessage : String -> Message
errorMessage text =
    { initMessage
        | messageClass = "negative"
        , header = "Error"
        , text = text
        , active = True
    }


successMessage : String -> Message
successMessage text =
    { initMessage
        | messageClass = "positive"
        , header = "Success"
        , text = text
        , active = True
    }


type alias Material =
    { id : String
    , name : String
    , unit_id : Maybe String
    , inventory : Float
    }


initMaterial : Material
initMaterial =
    { id = ""
    , name = ""
    , unit_id = Nothing
    , inventory = 0.0
    }


getMaterialByIdFromList : List Material -> String -> Maybe Material
getMaterialByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getMaterialByIdFromList xs id


type alias MaterialDB =
    { id : String
    , name : String
    , unit_id : Maybe String
    , inventory : Float
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
