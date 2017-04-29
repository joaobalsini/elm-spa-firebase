module Materials.Model exposing (..)

import Utils.StringNumberConversion exposing (floatToStringBrazilFormat, stringToFloatBrazilFormat)


type Subpage
    = IndexPage
    | NewPage
    | EditPage Material
    | ShowPage Material


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


type alias MaterialFormFields =
    { name : String
    , unit_id : String
    , inventory : String
    }


initMaterialFormFields : MaterialFormFields
initMaterialFormFields =
    { name = ""
    , unit_id = ""
    , inventory = ""
    }


type alias MaterialFormErrors =
    { name : Maybe String
    , unit_id : Maybe String
    , inventory : Maybe String
    }


formatMaterialFromSystemToForm : Material -> MaterialFormFields
formatMaterialFromSystemToForm materialFromSystem =
    let
        --put here any formattings from your system to your form (i.e dates, types, etc.)
        unit_id =
            case materialFromSystem.unit_id of
                Nothing ->
                    ""

                Just unit_id ->
                    unit_id

        materialFormFields =
            { initMaterialFormFields
                | name = materialFromSystem.name
                , unit_id = unit_id
                , inventory = floatToStringBrazilFormat materialFromSystem.inventory
            }
    in
        materialFormFields


parseMaterialFormToDb : String -> MaterialFormFields -> MaterialDB
parseMaterialFormToDb materialId materialFormFields =
    let
        --put here any parsings from your form to your system (i.e dates, types, etc.)
        inventoryFloat =
            case stringToFloatBrazilFormat materialFormFields.inventory of
                Nothing ->
                    0

                Just float ->
                    float

        unit_id =
            if String.length materialFormFields.unit_id == 0 then
                Nothing
            else
                Just materialFormFields.unit_id

        materialSystem =
            { id = materialId
            , name = materialFormFields.name
            , unit_id = unit_id
            , inventory = inventoryFloat
            }
    in
        materialSystem


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


updateMaterialAtId : Material -> String -> Material -> Material
updateMaterialAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


initMaterialFormErrors : MaterialFormErrors
initMaterialFormErrors =
    { name = Nothing
    , unit_id = Nothing
    , inventory = Nothing
    }


type alias Model =
    { materialFormFields : MaterialFormFields
    , materialFormErrors : MaterialFormErrors
    , materialFormShowErrorPanel : Bool
    , requestRemoveConfirmation : Maybe String
    }



-- real model


initModel : Model
initModel =
    { materialFormFields = initMaterialFormFields
    , materialFormErrors = initMaterialFormErrors
    , materialFormShowErrorPanel = False
    , requestRemoveConfirmation = Nothing
    }


validateMaterialFormName : String -> Result String String
validateMaterialFormName name =
    if String.length name >= 1 then
        (Ok name)
    else
        (Err "Should be filled")


validateMaterialFormInventory : String -> Result String String
validateMaterialFormInventory inventory =
    case stringToFloatBrazilFormat inventory of
        Nothing ->
            (Err "Should be a valid number")

        Just float ->
            if float > 0 then
                (Ok inventory)
            else
                (Err "Should be a number greater than 0")


validateMaterialFormUnitId : String -> Result String String
validateMaterialFormUnitId unitId =
    if String.length unitId >= 1 then
        (Ok unitId)
    else
        (Err "Should be selected")


processMaterialFormNameInput : String -> Model -> Model
processMaterialFormNameInput nameFromForm model =
    let
        actualMaterialFormFields =
            model.materialFormFields

        actualMaterialFormErrors =
            model.materialFormErrors

        -- here we are could reformat the variable to form
        ( nameFormError, nameNewFormValue ) =
            case (validateMaterialFormName nameFromForm) of
                Ok name ->
                    ( Nothing, name )

                Err error ->
                    ( Just error, nameFromForm )
    in
        { model | materialFormFields = { actualMaterialFormFields | name = nameNewFormValue }, materialFormErrors = { actualMaterialFormErrors | name = nameFormError } }


processMaterialFormUnitIdInput : String -> Model -> Model
processMaterialFormUnitIdInput unitIdFromForm model =
    let
        actualMaterialFormFields =
            model.materialFormFields

        actualMaterialFormErrors =
            model.materialFormErrors

        -- here we are could reformat the variable to form
        ( unitIdFormError, unitIdNewFormValue ) =
            case (validateMaterialFormUnitId unitIdFromForm) of
                Ok unitId ->
                    ( Nothing, unitId )

                Err error ->
                    ( Just error, unitIdFromForm )
    in
        { model | materialFormFields = { actualMaterialFormFields | unit_id = unitIdNewFormValue }, materialFormErrors = { actualMaterialFormErrors | unit_id = unitIdFormError } }


processMaterialFormInventoryInput : String -> Model -> Model
processMaterialFormInventoryInput inventoryFromForm model =
    let
        actualMaterialFormFields =
            model.materialFormFields

        actualMaterialFormErrors =
            model.materialFormErrors

        ( inventoryFormError, inventoryNewFormValue ) =
            case (validateMaterialFormInventory inventoryFromForm) of
                Ok inventoryFloat ->
                    ( Nothing, inventoryFromForm )

                Err error ->
                    ( Just error, inventoryFromForm )
    in
        { model | materialFormFields = { actualMaterialFormFields | inventory = inventoryNewFormValue }, materialFormErrors = { actualMaterialFormErrors | inventory = inventoryFormError } }


processAllMaterialFormFields : Model -> Model
processAllMaterialFormFields model =
    let
        updatedModel =
            model
                |> processMaterialFormNameInput model.materialFormFields.name
                |> processMaterialFormUnitIdInput model.materialFormFields.unit_id
                |> processMaterialFormInventoryInput model.materialFormFields.inventory
    in
        updatedModel
