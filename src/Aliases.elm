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


type alias MaterialDB =
    { id : String
    , name : String
    , unit_id : Maybe String
    , inventory : Float
    }


parseMaterialFromDB : MaterialDB -> Material
parseMaterialFromDB materialDb =
    let
        -- which fields needs special parsing first?
        material : Material
        material =
            { id = materialDb.id
            , name = materialDb.name
            , unit_id = materialDb.unit_id
            , inventory = materialDb.inventory
            }
    in
        material



-- format from system to db (trust the system)


formatMaterialToDB : Material -> MaterialDB
formatMaterialToDB material =
    let
        -- which fields needs special parsing first?
        materialDb : MaterialDB
        materialDb =
            { id = material.id
            , name = material.name
            , unit_id = material.unit_id
            , inventory = material.inventory
            }
    in
        materialDb


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
