port module Material exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Aliases exposing (..)
import Navigation
import Routes exposing (..)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)


myLocale : Locale
myLocale =
    { decimals = 2
    , thousandSeparator = "."
    , decimalSeparator = ","
    }



-- model
--form models


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


parseMaterialFromSystemToForm : Material -> MaterialFormFields
parseMaterialFromSystemToForm materialFromSystem =
    let
        --put here any formattings from your system to your form (i.e dates, types, etc.)
        materialFormFields =
            { initMaterialFormFields
                | name = materialFromSystem.name
                , unit_id = Maybe.withDefault "" materialFromSystem.unit_id
                , inventory = format myLocale materialFromSystem.inventory
            }
    in
        materialFormFields


parseMaterialFormToSystem : String -> MaterialFormFields -> Material
parseMaterialFormToSystem materialId materialFormFields =
    let
        --put here any parsings from your form to your system (i.e dates, types, etc.)
        inventoryFloat =
            case Aliases.stringToFloatBrazilFormat materialFormFields.inventory of
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
    , redirectRoute : Route
    , waitingServerConfirmation : Bool
    }


type Subpage
    = IndexPage
    | NewPage
    | EditPage String
    | ShowPage String


subpageToHash : Subpage -> String
subpageToHash subpage =
    case subpage of
        IndexPage ->
            "#/materials"

        NewPage ->
            "#/materials/new"

        ShowPage id ->
            "#/materials/" ++ id

        EditPage id ->
            "#/materials/edit/" ++ id



-- real model


initModel : Model
initModel =
    { materialFormFields = initMaterialFormFields
    , materialFormErrors = initMaterialFormErrors
    , materialFormShowErrorPanel = False
    , redirectRoute = Routes.MaterialIndexRoute
    , waitingServerConfirmation = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = Navigate Subpage (List Material) (List Unit) Message
    | NameInputChanged String
    | UnitSelectChanged String
    | InventoryInputChanged String
    | SubmitMaterialForm String
    | Added Material
    | Updated Material
    | Remove Material
    | Removed String


processMaterialFormNameInput : String -> Model -> Model
processMaterialFormNameInput nameFromForm model =
    let
        actualMaterialFormFields =
            model.materialFormFields

        actualMaterialFormErrors =
            model.materialFormErrors

        -- here we are could reformat the variable to form
        ( nameFormError, nameNewFormValue ) =
            if String.length nameFromForm < 1 then
                ( Just "Should be filled", nameFromForm )
            else
                ( Nothing, nameFromForm )
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
            if String.length unitIdFromForm < 1 then
                ( Just "Should be selected", unitIdFromForm )
            else
                ( Nothing, unitIdFromForm )
    in
        { model | materialFormFields = { actualMaterialFormFields | unit_id = unitIdNewFormValue }, materialFormErrors = { actualMaterialFormErrors | unit_id = unitIdFormError } }


processMaterialFormInventoryInput : String -> Model -> Model
processMaterialFormInventoryInput inventoryFromForm model =
    let
        actualMaterialFormFields =
            model.materialFormFields

        actualMaterialFormErrors =
            model.materialFormErrors

        inventoryFloat =
            case Aliases.stringToFloatBrazilFormat inventoryFromForm of
                Nothing ->
                    0

                Just float ->
                    float

        inventoryFormError =
            if inventoryFloat <= 0 then
                Just "Must Enter a Positive Number"
            else
                Nothing
    in
        { model | materialFormFields = { actualMaterialFormFields | inventory = inventoryFromForm }, materialFormErrors = { actualMaterialFormErrors | inventory = inventoryFormError } }


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


getMaterialByIdFromMaterialsList : List Material -> String -> Maybe Material
getMaterialByIdFromMaterialsList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getMaterialByIdFromMaterialsList xs id


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        Navigate subpage materials units message ->
            case subpage of
                ShowPage id ->
                    ( model
                    , Navigation.newUrl <| subpageToHash subpage
                    , initMessage
                    )

                EditPage id ->
                    let
                        materialOrInitMaterial =
                            Maybe.withDefault initMaterial (getMaterialByIdFromMaterialsList materials id)

                        updatedModel =
                            { model | materialFormFields = parseMaterialFromSystemToForm materialOrInitMaterial }
                                |> processAllMaterialFormFields
                    in
                        ( updatedModel
                        , Navigation.newUrl <| subpageToHash subpage
                        , initMessage
                        )

                _ ->
                    let
                        unit_id =
                            case List.head units of
                                Nothing ->
                                    ""

                                Just unit ->
                                    unit.id

                        updatedMaterialFormFields =
                            { initMaterialFormFields | unit_id = unit_id }
                    in
                        ( { initModel | materialFormFields = updatedMaterialFormFields }
                        , Navigation.newUrl <| subpageToHash subpage
                        , initMessage
                        )

        NameInputChanged name ->
            ( processMaterialFormNameInput name model, Cmd.none, initMessage )

        InventoryInputChanged inventory ->
            ( processMaterialFormInventoryInput inventory model, Cmd.none, initMessage )

        UnitSelectChanged unit_id ->
            ( processMaterialFormUnitIdInput unit_id model, Cmd.none, initMessage )

        SubmitMaterialForm maybeId ->
            let
                --simulate changes in all form fields, to see if it they are valid if they didn't change
                -- ( model1, _, _ ) =
                --     update (MaterialFormNameInput model.materialFormName) model
                processedModel =
                    model
                        |> processAllMaterialFormFields

                showError =
                    processedModel.materialFormErrors.name
                        /= Nothing
                        || processedModel.materialFormErrors.unit_id
                        /= Nothing
                        || processedModel.materialFormErrors.inventory
                        /= Nothing

                ( updatedModel, cmd ) =
                    if showError then
                        ( { processedModel | materialFormShowErrorPanel = True }, Cmd.none )
                    else
                        let
                            materialToDb : MaterialDB
                            materialToDb =
                                model.materialFormFields
                                    |> parseMaterialFormToSystem maybeId
                                    |> formatMaterialToDB
                        in
                            if maybeId == "" then
                                ( { initModel | waitingServerConfirmation = True }, addMaterial materialToDb )
                            else
                                ( { initModel | waitingServerConfirmation = True }, updateMaterial materialToDb )
            in
                ( updatedModel, cmd, initMessage )

        Added material ->
            let
                message =
                    if model.waitingServerConfirmation then
                        successMessage "Material successfully added"
                    else
                        initMessage
            in
                ( model, Navigation.newUrl "#/materials", message )

        Remove material ->
            ( model, removeMaterial material, initMessage )

        Removed id ->
            let
                message =
                    if model.waitingServerConfirmation then
                        successMessage "Material successfully removed"
                    else
                        initMessage
            in
                ( model, Cmd.none, message )

        Updated material ->
            -- create update material
            let
                message =
                    if model.waitingServerConfirmation then
                        successMessage "Material successfully updated"
                    else
                        initMessage
            in
                ( model, Navigation.newUrl "#/materials", message )


view : Model -> List Material -> List Unit -> Subpage -> Html Msg
view model materials units subpage =
    let
        page =
            case subpage of
                IndexPage ->
                    materialsToTable materials units model

                NewPage ->
                    materialForm model units Nothing

                EditPage string ->
                    materialForm model units (Just string)

                ShowPage string ->
                    materialShow model string
    in
        div [ class "main" ]
            [ materialFormErrorPanel model
            , page
            ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                , button [ type_ "button" ] [ text "×" ]
                ]


unitOption : String -> Unit -> Html Msg
unitOption selectedUnitId unit =
    option [ value unit.id, selected (selectedUnitId == unit.id) ] [ text (unit.name ++ "(" ++ unit.initials ++ ")") ]


materialsToTable : List Material -> List Unit -> Model -> Html Msg
materialsToTable materials units model =
    -- filter using query afterwards
    let
        table_ =
            List.map (materialToTr materials units) materials
                |> tbody []
                |> (\r -> materialsTh :: [ r ])
                |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Materials list" ]
            , table_
            , button [ class "ui button", onClick (Navigate NewPage materials units initMessage) ] [ text "New" ]
            ]


materialsTh : Html Msg
materialsTh =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Unit ID" ]
            , th [] [ text "Inventory" ]
            , th [] [ text "Actions" ]
            ]
        ]


materialToTr : List Material -> List Unit -> Material -> Html Msg
materialToTr materials units material =
    tr []
        [ td [] [ text material.id ]
        , td [] [ text material.name ]
        , td [] [ text (Maybe.withDefault "" material.unit_id) ]
        , td [] [ text <| toString material.inventory ]
        , td []
            [ button [ class "ui button", onClick (Navigate (EditPage material.id) materials units initMessage) ] [ text "Edit" ]
            , button [ class "ui button", onClick (Navigate (ShowPage material.id) materials units initMessage) ] [ text "Show" ]
            , button [ class "ui button", onClick (Remove material) ] [ text "Remove" ]
            ]
        ]


materialShow : Model -> String -> Html Msg
materialShow model key =
    div [] [ text ("Showing material id:" ++ key) ]


materialForm : Model -> List Unit -> Maybe String -> Html Msg
materialForm model units maybeKey =
    let
        headerMessage =
            if maybeKey == Nothing then
                "New material"
            else
                "Editing material with id: " ++ (Maybe.withDefault "" maybeKey)
    in
        Html.form [ class "ui large form", onSubmit (SubmitMaterialForm (Maybe.withDefault "" maybeKey)) ]
            [ div [ class "ui stacked segment" ]
                [ h1 [] [ text headerMessage ]
                , div
                    [ classList
                        [ ( "field", True ), ( "error", model.materialFormErrors.name /= Nothing ) ]
                    ]
                    [ label [] [ text "Name" ]
                    , input
                        [ type_ "text"
                        , value model.materialFormFields.name
                        , onInput NameInputChanged
                        ]
                        []
                    , span [] [ text <| Maybe.withDefault "" model.materialFormErrors.name ]
                    ]
                , div
                    [ classList
                        [ ( "field", True ), ( "error", model.materialFormErrors.unit_id /= Nothing ) ]
                    ]
                    [ label [] [ text "UnitId" ]
                    , select [ onInput UnitSelectChanged ]
                        (List.map (unitOption model.materialFormFields.unit_id) units)
                    , span [] [ text <| Maybe.withDefault "" model.materialFormErrors.unit_id ]
                    ]
                , div
                    [ classList
                        [ ( "field", True ), ( "error", model.materialFormErrors.inventory /= Nothing ) ]
                    ]
                    [ label [] [ text "Inventory" ]
                    , input
                        [ type_ "text"
                        , value model.materialFormFields.inventory
                        , onInput InventoryInputChanged
                        ]
                        []
                    , span [] [ text <| Maybe.withDefault "" model.materialFormErrors.inventory ]
                    ]
                , div []
                    [ label [] []
                    , button [ type_ "submit", class "ui fluid large teal submit button" ] [ text "Save" ]
                    ]
                ]
            ]


materialFormErrorPanel : Model -> Html a
materialFormErrorPanel model =
    let
        list : List FieldError
        list =
            [ { fieldName = "Name", errorMessage = model.materialFormErrors.name }
            , { fieldName = "UnitId", errorMessage = model.materialFormErrors.unit_id }
            , { fieldName = "Inventory", errorMessage = model.materialFormErrors.inventory }
            ]

        elementsWithError : List FieldError
        elementsWithError =
            List.filter (\el -> el.errorMessage /= Nothing) list
    in
        if model.materialFormShowErrorPanel then
            div [ id "formErrors", class "ui message error " ]
                [ div [ class "header" ] [ text "We had some issues:" ]
                , ul [ class "list" ] (List.map (\el -> li [] [ text (el.fieldName ++ ":" ++ (Maybe.withDefault "" el.errorMessage)) ]) elementsWithError)
                ]
        else
            div [] []


port addMaterial : Material -> Cmd msg


port materialAdded : (Material -> msg) -> Sub msg


port updateMaterial : Material -> Cmd msg


port materialUpdated : (Material -> msg) -> Sub msg


port removeMaterial : Material -> Cmd msg


port materialRemoved : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ materialAdded Added
        , materialUpdated Updated
        , materialRemoved Removed
        ]
