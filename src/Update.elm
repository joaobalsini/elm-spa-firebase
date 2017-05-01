module Update exposing (..)

import Navigation
import Routes exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg, ReturnMsg(..))
import IndexModule
import LoginModule
import Units.Update
import Units.Msgs
import Materials.Msgs
import Materials.Update
import Store.Update
import NotificationModule exposing (initNotification, errorNotification, successNotification)
import Materials.Model exposing (Material, MaterialDB, getMaterialByIdFromList, parseMaterialFromDB, updateMaterialAtId)
import Units.Model exposing (Unit, UnitDB, getUnitByIdFromList, parseUnitFromDB, updateUnitAtId)
import Store.Commands
import Materials.Routes
import Units.Routes


processReturnMsg : ReturnMsg -> Model -> Model
processReturnMsg returnMsg model =
    let
        newReturnMsgsToProcess =
            case returnMsg of
                NoOp ->
                    model.returnMsgsToProcess

                ShowNotification string ->
                    model.returnMsgsToProcess

                WaitForServerSuccessAndRedirectWithDefaultRouteAndNotification route string ->
                    if List.length (model.returnMsgsToProcess) > 0 then
                        model.returnMsgsToProcess
                    else
                        [ WaitForServerSuccessAndRedirectToRouteWithNotification route string ]

                _ ->
                    model.returnMsgsToProcess ++ [ returnMsg ]

        waitingServerResponse =
            case returnMsg of
                NoOp ->
                    False

                ShowNotification string ->
                    False

                _ ->
                    True

        notification =
            case returnMsg of
                ShowNotification string ->
                    successNotification string

                _ ->
                    initNotification
    in
        { model | returnMsgsToProcess = newReturnMsgsToProcess, waitingServerResponse = waitingServerResponse, notification = notification }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Navigate is used once a user clicks in a link
        Msgs.Navigate route ->
            let
                ( model_, msg_ ) =
                    update (Msgs.ChangePage route) model
            in
                ( model_, Navigation.newUrl <| routeToHash route )

        -- ChangePage is used once a user changes the URL manually
        Msgs.ChangePage route ->
            case route of
                -- We need to process part of the notification here, as the array of units is "Main module" responsability
                -- Why don't we keep the array of units inside unitModel?
                -- Because some other modules will also need it and we want to avoid duplications as it would tend to bug
                -- We could also create "notification interceptors" for each notification of the subModule but I only need to pre process some of them
                -- As the UnitShow and UnitEdit need to be pre validated (as the user could, for example)
                -- And, in case of Unit Edit we NEED to set the unit inside the unit module as we need to interact of it inside the unit view, when inputsChanged for example
                UnitsRoutes subroute ->
                    let
                        ( updatedModel, cmd ) =
                            -- load entities as needed - if not loaded, trigger the store command to load
                            case model.store.units of
                                Nothing ->
                                    case subroute of
                                        -- need to reload the route to reprocess the form
                                        Units.Routes.UnitEditRoute id ->
                                            let
                                                newReturnMsgsToProcess =
                                                    model.returnMsgsToProcess ++ [ WaitForServerSuccessAndRedirectToRoute route ]
                                            in
                                                ( { model | route = route, returnMsgsToProcess = newReturnMsgsToProcess }, Store.Commands.loadUnits () )

                                        _ ->
                                            ( { model | route = route }, Store.Commands.loadUnits () )

                                Just units ->
                                    case subroute of
                                        Units.Routes.UnitEditRoute id ->
                                            let
                                                maybeUnit =
                                                    getUnitByIdFromList units id
                                            in
                                                case maybeUnit of
                                                    Nothing ->
                                                        ( model, Cmd.none )

                                                    Just unit ->
                                                        let
                                                            ( unitModel, cmd, returnMsg ) =
                                                                -- send msg LoadFormData ShowPage to UnitModule passing units and initNotification
                                                                Units.Update.update (Units.Msgs.LoadFormData unit) model.unitModule
                                                        in
                                                            ( { model | route = route, unitModule = unitModel }, Cmd.none )

                                        _ ->
                                            let
                                                ( unitModel, cmd, returnMsg ) =
                                                    -- send msg LoadFormData ShowPage to UnitModule passing units and initNotification
                                                    Units.Update.update (Units.Msgs.ClearFormData) model.unitModule
                                            in
                                                ( { model | route = route, unitModule = unitModel }, Cmd.none )
                    in
                        ( updatedModel, cmd )

                MaterialsRoutes subroute ->
                    let
                        ( updatedModel, cmd ) =
                            -- load entities as needed - if not loaded, trigger the store command to load
                            case model.store.materials of
                                Nothing ->
                                    let
                                        cmd =
                                            if model.store.units == Nothing then
                                                Cmd.batch [ Store.Commands.loadUnits (), Store.Commands.loadMaterials () ]
                                            else
                                                Store.Commands.loadMaterials ()
                                    in
                                        -- need to reload the route to reprocess the form
                                        case subroute of
                                            Materials.Routes.MaterialEditRoute id ->
                                                let
                                                    newReturnMsgsToProcess =
                                                        model.returnMsgsToProcess ++ [ WaitForServerSuccessAndRedirectToRoute route ]
                                                in
                                                    ( { model | route = route, returnMsgsToProcess = newReturnMsgsToProcess }, cmd )

                                            _ ->
                                                ( { model | route = route }, cmd )

                                Just materials ->
                                    case subroute of
                                        Materials.Routes.MaterialEditRoute id ->
                                            let
                                                maybeMaterial =
                                                    getMaterialByIdFromList materials id
                                            in
                                                case maybeMaterial of
                                                    Nothing ->
                                                        ( model, Cmd.none )

                                                    Just material ->
                                                        let
                                                            ( materialModel, cmd, returnMsg ) =
                                                                -- send msg PrepareView ShowPage to MaterialModule passing materials and initNotification
                                                                Materials.Update.update (Materials.Msgs.LoadFormData material) model.materialModule
                                                        in
                                                            ( { model | route = route, materialModule = materialModel }, Cmd.none )

                                        _ ->
                                            let
                                                ( materialModel, cmd, returnMsg ) =
                                                    -- send msg LoadFormData ShowPage to UnitModule passing units and initNotification
                                                    Materials.Update.update (Materials.Msgs.ClearFormData) model.materialModule
                                            in
                                                ( { model | route = route, materialModule = materialModel }, Cmd.none )
                    in
                        ( updatedModel, cmd )

                _ ->
                    ( { model | route = route }, Cmd.none )

        Msgs.IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    IndexModule.update msg model.indexModule
            in
                ( { model | indexModule = indexModel }
                , Cmd.map Msgs.IndexMsg cmd
                )

        Msgs.LoginMsg msg ->
            let
                ( loginModel, cmd ) =
                    LoginModule.update msg model.loginModule
            in
                ( { model | loginModule = loginModel }
                , Cmd.map Msgs.LoginMsg cmd
                )

        Msgs.UnitMsg msg ->
            let
                ( unitModel, cmd, returnMsg ) =
                    Units.Update.update msg model.unitModule

                modelAfterProcessingReturnMsgs =
                    { model | unitModule = unitModel }
                        |> processReturnMsg returnMsg
            in
                ( modelAfterProcessingReturnMsgs
                , Cmd.map Msgs.UnitMsg cmd
                )

        Msgs.MaterialMsg msg ->
            let
                ( materialModel, cmd, returnMsg ) =
                    Materials.Update.update msg model.materialModule

                modelAfterProcessingReturnMsgs =
                    { model | materialModule = materialModel }
                        |> processReturnMsg returnMsg
            in
                ( modelAfterProcessingReturnMsgs
                , Cmd.map Msgs.MaterialMsg cmd
                )

        Msgs.NotificationMsg msg ->
            let
                ( notificationModel, cmd ) =
                    NotificationModule.update msg model.notification
            in
                ( { model | notification = notificationModel }
                , Cmd.map Msgs.NotificationMsg cmd
                )

        Msgs.StoreMsg msg ->
            let
                ( storeModel, cmd ) =
                    Store.Update.update msg model.store

                updatedModel =
                    { model | store = storeModel, waitingServerResponse = False }

                ( resultModel, resultCmd ) =
                    case model.returnMsgsToProcess of
                        [] ->
                            ( { updatedModel | returnMsgsToProcess = [] }, Cmd.none )

                        x :: xs ->
                            case x of
                                WaitForServerSuccessAndRedirectToRoute route ->
                                    let
                                        resultCmd =
                                            Cmd.batch [ Cmd.map Msgs.StoreMsg cmd, Navigation.newUrl <| routeToHash route ]

                                        resultModel =
                                            { updatedModel | returnMsgsToProcess = xs }
                                    in
                                        ( resultModel, resultCmd )

                                WaitForServerSuccessAndRedirectWithDefaultRouteAndNotification route notification ->
                                    let
                                        resultCmd =
                                            Cmd.batch [ Cmd.map Msgs.StoreMsg cmd, Navigation.newUrl <| routeToHash route ]

                                        resultModel =
                                            { updatedModel | returnMsgsToProcess = xs, notification = successNotification notification }
                                    in
                                        ( resultModel, resultCmd )

                                WaitForServerSuccessAndRedirectToRouteWithNotification route notification ->
                                    let
                                        resultCmd =
                                            Cmd.batch [ Cmd.map Msgs.StoreMsg cmd, Navigation.newUrl <| routeToHash route ]

                                        resultModel =
                                            { updatedModel | returnMsgsToProcess = xs, notification = successNotification notification }
                                    in
                                        ( resultModel, resultCmd )

                                WaitForServerSuccessAndShowNotification notification ->
                                    let
                                        resultCmd =
                                            Cmd.map Msgs.StoreMsg cmd

                                        resultModel =
                                            { updatedModel | returnMsgsToProcess = xs, notification = successNotification notification }
                                    in
                                        ( resultModel, resultCmd )

                                WaitForServerSuccessAndRedirectToRouteWithNotificationRestoringMaterialModel route notification materialModel ->
                                    let
                                        resultCmd =
                                            Cmd.map Msgs.StoreMsg cmd

                                        resultModel =
                                            { updatedModel | notification = successNotification notification, returnMsgsToProcess = xs, materialModule = materialModel }
                                    in
                                        ( resultModel, resultCmd )

                                _ ->
                                    let
                                        resultCmd =
                                            Cmd.map Msgs.StoreMsg cmd

                                        resultModel =
                                            updatedModel
                                    in
                                        ( resultModel, resultCmd )
            in
                ( resultModel
                , resultCmd
                )
