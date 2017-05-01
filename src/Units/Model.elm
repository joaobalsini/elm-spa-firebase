module Units.Model exposing (..)


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


type alias UnitFormFields =
    { name : String
    , initials : String
    }


initUnitFormFields : UnitFormFields
initUnitFormFields =
    { name = ""
    , initials = ""
    }


type alias UnitFormErrors =
    { name : Maybe String
    , initials : Maybe String
    }


formatUnitFromSystemToForm : Unit -> UnitFormFields
formatUnitFromSystemToForm unitFromSystem =
    let
        --put here any formattings from your system to your form (i.e dates, types, etc.)
        unitFormFields =
            { initUnitFormFields
                | name = unitFromSystem.name
                , initials = unitFromSystem.initials
            }
    in
        unitFormFields


parseUnitFormToDb : String -> UnitFormFields -> UnitDB
parseUnitFormToDb unitId unitFormFields =
    let
        --put here any parsings from your form to your system (i.e dates, types, etc.)
        unitSystem =
            { id = unitId
            , name = unitFormFields.name
            , initials = unitFormFields.initials
            }
    in
        unitSystem


parseUnitFromDB : UnitDB -> Unit
parseUnitFromDB unitDb =
    let
        -- which fields needs special parsing first?
        unit : Unit
        unit =
            { id = unitDb.id
            , name = unitDb.name
            , initials = unitDb.initials
            }
    in
        unit


updateUnitAtId : Unit -> String -> Unit -> Unit
updateUnitAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


initUnitFormErrors : UnitFormErrors
initUnitFormErrors =
    { name = Nothing
    , initials = Nothing
    }


type alias Model =
    { unitFormFields : UnitFormFields
    , unitFormErrors : UnitFormErrors
    , unitFormShowErrorPanel : Bool
    , requestRemoveConfirmation : Maybe String
    }



-- real model


initModel : Model
initModel =
    { unitFormFields = initUnitFormFields
    , unitFormErrors = initUnitFormErrors
    , unitFormShowErrorPanel = False
    , requestRemoveConfirmation = Nothing
    }


unitChanged : Maybe Unit -> UnitFormFields -> Bool
unitChanged maybeUnitParsedFromDB actualUnitForm =
    case maybeUnitParsedFromDB of
        Nothing ->
            True

        Just unit ->
            let
                unitFormattedToForm =
                    formatUnitFromSystemToForm unit
            in
                if unitFormattedToForm.name == actualUnitForm.name && unitFormattedToForm.initials == actualUnitForm.initials then
                    False
                else
                    True
