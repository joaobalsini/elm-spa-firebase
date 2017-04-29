module Materials.Update exposing (..)

import Navigation
import Routes exposing (Route, routeToHash)
import Materials.Model exposing (Model, Material, MaterialDB, initModel, initMaterialFormFields)
import Materials.Msgs exposing (..)
import Store.Commands
import Materials.Routes


-- update


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Route, Bool )
update msg model =
    case msg of
        LoadFormData material ->
            let
                updatedModel =
                    { model | materialFormFields = Materials.Model.formatMaterialFromSystemToForm material }
                        |> Materials.Model.processAllMaterialFormFields
            in
                ( updatedModel
                , Cmd.none
                , Nothing
                , False
                )

        ClearFormData ->
            ( { model | materialFormFields = initMaterialFormFields }
            , Cmd.none
            , Nothing
            , False
            )

        NavigateRoute route ->
            ( model
            , Navigation.newUrl <| routeToHash route
            , Nothing
            , False
            )

        RedirectBack ->
            ( model
            , Navigation.back 1
            , Nothing
            , False
            )

        NameInputChanged name ->
            ( Materials.Model.processMaterialFormNameInput name model, Cmd.none, Nothing, False )

        InventoryInputChanged inventory ->
            ( Materials.Model.processMaterialFormInventoryInput inventory model, Cmd.none, Nothing, False )

        UnitSelectChanged unit_id ->
            ( Materials.Model.processMaterialFormUnitIdInput unit_id model, Cmd.none, Nothing, False )

        SubmitMaterialForm maybeId ->
            let
                --simulate changes in all form fields, to see if it they are valid if they didn't change
                -- ( model1, _, _ ) =
                --     update (MaterialFormNameInput model.materialFormName) model
                processedModel =
                    model
                        |> Materials.Model.processAllMaterialFormFields

                showError =
                    processedModel.materialFormErrors.name
                        /= Nothing
                        || processedModel.materialFormErrors.unit_id
                        /= Nothing
                        || processedModel.materialFormErrors.inventory
                        /= Nothing

                ( updatedModel, cmd, redirectRoute, waitingServerResponse ) =
                    if showError then
                        ( { processedModel | materialFormShowErrorPanel = True }, Cmd.none, Nothing, False )
                    else
                        let
                            materialToDb : MaterialDB
                            materialToDb =
                                model.materialFormFields
                                    |> Materials.Model.parseMaterialFormToDb maybeId
                        in
                            if maybeId == "" then
                                ( initModel, Store.Commands.addMaterial materialToDb, Just (Routes.MaterialsRoutes Materials.Routes.MaterialIndexRoute), True )
                            else
                                ( initModel, Store.Commands.updateMaterial materialToDb, Just (Routes.MaterialsRoutes Materials.Routes.MaterialIndexRoute), True )
            in
                ( updatedModel, cmd, redirectRoute, waitingServerResponse )

        RequestRemoveConfirmation id ->
            ( { model | requestRemoveConfirmation = Just id }, Cmd.none, Nothing, False )

        CancelRemoveConfirmation ->
            ( { model | requestRemoveConfirmation = Nothing }, Cmd.none, Nothing, False )

        Remove id ->
            ( model, Store.Commands.removeMaterial id, Nothing, True )
