module Materials.Update exposing (..)

import Navigation
import Routes exposing (Route, routeToHash)
import Materials.Model exposing (Model, Material, MaterialDB, initModel, initMaterialFormFields, initMaterialFormErrors, materialChanged)
import Materials.Msgs exposing (..)
import Store.Commands
import Materials.Routes
import ReturnMsgs exposing (ReturnMsg(..))


-- update


update : Msg -> Model -> ( Model, Cmd Msg, ReturnMsg )
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
                , NoOp
                )

        ClearFormData ->
            ( { model | materialFormFields = initMaterialFormFields, materialFormErrors = initMaterialFormErrors, materialFormShowErrorPanel = False }
            , Cmd.none
            , NoOp
            )

        NavigateRoute route returnMsg ->
            ( model
            , Navigation.newUrl <| routeToHash route
            , returnMsg
            )

        RedirectBack ->
            ( model
            , Navigation.back 1
            , NoOp
            )

        NameInputChanged name ->
            ( Materials.Model.processMaterialFormNameInput name model, Cmd.none, NoOp )

        InventoryInputChanged inventory ->
            ( Materials.Model.processMaterialFormInventoryInput inventory model, Cmd.none, NoOp )

        UnitSelectChanged unit_id ->
            ( Materials.Model.processMaterialFormUnitIdInput unit_id model, Cmd.none, NoOp )

        SubmitMaterialForm maybeMaterial ->
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

                ( updatedModel, cmd, returnMsg ) =
                    if showError then
                        ( { processedModel | materialFormShowErrorPanel = True }, Cmd.none, NoOp )
                    else if (materialChanged maybeMaterial processedModel.materialFormFields) then
                        let
                            idOrEmptyId =
                                case maybeMaterial of
                                    Nothing ->
                                        ""

                                    Just material ->
                                        material.id

                            materialToDb : MaterialDB
                            materialToDb =
                                model.materialFormFields
                                    |> Materials.Model.parseMaterialFormToDb idOrEmptyId
                        in
                            case maybeMaterial of
                                Nothing ->
                                    ( initModel, Store.Commands.addMaterial materialToDb, WaitForServerSuccessAndRedirectWithDefaultRouteAndNotification (Routes.MaterialsRoutes Materials.Routes.MaterialIndexRoute) "Material successfully added!" )

                                Just material ->
                                    ( initModel, Store.Commands.updateMaterial materialToDb, WaitForServerSuccessAndRedirectWithDefaultRouteAndNotification (Routes.MaterialsRoutes Materials.Routes.MaterialIndexRoute) "Material successfully updated!" )
                    else
                        ( model
                        , Navigation.back 1
                        , ShowNotification "Nothing changed"
                        )
            in
                ( updatedModel, cmd, returnMsg )

        RequestRemoveConfirmation id ->
            ( { model | requestRemoveConfirmation = Just id }, Cmd.none, NoOp )

        CancelRemoveConfirmation ->
            ( { model | requestRemoveConfirmation = Nothing }, Cmd.none, NoOp )

        Remove id ->
            ( { model | requestRemoveConfirmation = Nothing }, Store.Commands.removeMaterial id, WaitForServerSuccessAndShowNotification "Material successfully removed!" )
