module Units.Update exposing (..)

import Navigation
import Utils.StringNumberConversion exposing (stringToFloatBrazilFormat)
import Routes exposing (Route, routeToHash)
import Units.Model exposing (Model, Unit, UnitDB, initModel, initUnitFormFields, initUnitFormErrors, formatUnitFromSystemToForm, parseUnitFormToDb, unitChanged)
import Units.Msgs exposing (..)
import Store.Commands
import Units.Routes
import Msgs exposing (ReturnMsg(..))


-- update


validateUnitFormName : String -> Result String String
validateUnitFormName name =
    if String.length name >= 1 then
        (Ok name)
    else
        (Err "Should be filled")


validateUnitFormInventory : String -> Result String String
validateUnitFormInventory inventory =
    case stringToFloatBrazilFormat inventory of
        Nothing ->
            (Err "Should be a valid number")

        Just float ->
            if float > 0 then
                (Ok inventory)
            else
                (Err "Should be a number greater than 0")


validateUnitFormUnitId : String -> Result String String
validateUnitFormUnitId unitId =
    if String.length unitId >= 1 then
        (Ok unitId)
    else
        (Err "Should be selected")


processUnitFormNameInput : String -> Model -> Model
processUnitFormNameInput nameFromForm model =
    let
        actualUnitFormFields =
            model.unitFormFields

        actualUnitFormErrors =
            model.unitFormErrors

        -- here we are could reformat the variable to form
        ( nameFormError, nameNewFormValue ) =
            case (validateUnitFormName nameFromForm) of
                Ok name ->
                    ( Nothing, name )

                Err error ->
                    ( Just error, nameFromForm )
    in
        { model | unitFormFields = { actualUnitFormFields | name = nameNewFormValue }, unitFormErrors = { actualUnitFormErrors | name = nameFormError } }


processUnitFormInitialsInput : String -> Model -> Model
processUnitFormInitialsInput initialsFromForm model =
    let
        actualUnitFormFields =
            model.unitFormFields

        actualUnitFormErrors =
            model.unitFormErrors

        -- here we are could reformat the variable to form
        ( initialsFormError, initialsNewFormValue ) =
            case (validateUnitFormName initialsFromForm) of
                Ok initials ->
                    ( Nothing, initials )

                Err error ->
                    ( Just error, initialsFromForm )
    in
        { model | unitFormFields = { actualUnitFormFields | initials = initialsNewFormValue }, unitFormErrors = { actualUnitFormErrors | initials = initialsFormError } }


processAllUnitFormFields : Model -> Model
processAllUnitFormFields model =
    let
        updatedModel =
            model
                |> processUnitFormNameInput model.unitFormFields.name
                |> processUnitFormInitialsInput model.unitFormFields.initials
    in
        updatedModel


update : Msg -> Model -> ( Model, Cmd Msg, ReturnMsg )
update msg model =
    case msg of
        LoadFormData unit ->
            let
                updatedModel =
                    { model | unitFormFields = formatUnitFromSystemToForm unit }
                        |> processAllUnitFormFields
            in
                ( updatedModel
                , Cmd.none
                , NoOp
                )

        ClearFormData ->
            ( { model | unitFormFields = initUnitFormFields, unitFormErrors = initUnitFormErrors, unitFormShowErrorPanel = False }
            , Cmd.none
            , NoOp
            )

        NavigateRoute route ->
            ( model
            , Navigation.newUrl <| routeToHash route
            , NoOp
            )

        RedirectBack ->
            ( model
            , Navigation.back 1
            , NoOp
            )

        NameInputChanged name ->
            ( processUnitFormNameInput name model, Cmd.none, NoOp )

        InitialsInputChanged name ->
            ( processUnitFormInitialsInput name model, Cmd.none, NoOp )

        SubmitUnitForm maybeUnit ->
            let
                --simulate changes in all form fields, to see if it they are valid if they didn't change
                -- ( model1, _, _ ) =
                --     update (UnitFormNameInput model.unitFormName) model
                processedModel =
                    model
                        |> processAllUnitFormFields

                showError =
                    processedModel.unitFormErrors.name
                        /= Nothing
                        || processedModel.unitFormErrors.initials
                        /= Nothing

                ( updatedModel, cmd, returnMsg ) =
                    if showError then
                        ( { processedModel | unitFormShowErrorPanel = True }, Cmd.none, NoOp )
                    else if (unitChanged maybeUnit processedModel.unitFormFields) then
                        let
                            idOrEmptyId =
                                case maybeUnit of
                                    Nothing ->
                                        ""

                                    Just unit ->
                                        unit.id

                            unitToDb : UnitDB
                            unitToDb =
                                model.unitFormFields
                                    |> parseUnitFormToDb idOrEmptyId
                        in
                            case maybeUnit of
                                Nothing ->
                                    ( initModel, Store.Commands.addUnit unitToDb, WaitForServerSuccessAndRedirectWithDefaultRouteAndNotification (Routes.UnitsRoutes Units.Routes.UnitIndexRoute) "Unit successfully added!" )

                                Just unit ->
                                    ( initModel, Store.Commands.updateUnit unitToDb, WaitForServerSuccessAndRedirectWithDefaultRouteAndNotification (Routes.UnitsRoutes Units.Routes.UnitIndexRoute) "Unit successfully updated!" )
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
            ( model, Store.Commands.removeUnit id, WaitForServerSuccessAndShowNotification "Unit successfully removed!" )
