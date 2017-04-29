module Update exposing (..)

import Navigation
import Routes exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg)
import IndexModule
import LoginModule
import Units.Update
import Units.Msgs
import Materials.Msgs
import Materials.Update
import Store.Update
import MessageModule exposing (initMessage, errorMessage, successMessage)
import Materials.Model exposing (Material, MaterialDB, getMaterialByIdFromList, parseMaterialFromDB, updateMaterialAtId)
import Units.Model exposing (Unit, UnitDB, getUnitByIdFromList, parseUnitFromDB, updateUnitAtId)
import Store.Commands
import Materials.Routes
import Units.Routes


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
                -- We need to process part of the message here, as the array of units is "Main module" responsability
                -- Why don't we keep the array of units inside unitModel?
                -- Because some other modules will also need it and we want to avoid duplications as it would tend to bug
                -- We could also create "message interceptors" for each message of the subModule but I only need to pre process some of them
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
                                            ( { model | route = route, redirectAfterServerResponse = Just route }, Store.Commands.loadUnits () )

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
                                                            ( unitModel, cmd, maybeRoute, waitingServerConfirmation ) =
                                                                -- send msg LoadFormData ShowPage to UnitModule passing units and initMessage
                                                                Units.Update.update (Units.Msgs.LoadFormData unit) model.unitModule
                                                        in
                                                            ( { model | route = route, unitModule = unitModel }, Cmd.none )

                                        _ ->
                                            ( { model | route = route }, Cmd.none )
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
                                                ( { model | route = route, redirectAfterServerResponse = Just route }, cmd )

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
                                                            ( materialModel, cmd, maybeRoute, waitingServerResponse ) =
                                                                -- send msg PrepareView ShowPage to MaterialModule passing materials and initMessage
                                                                Materials.Update.update (Materials.Msgs.LoadFormData material) model.materialModule
                                                        in
                                                            ( { model | route = route, materialModule = materialModel }, Cmd.none )

                                        _ ->
                                            ( { model | route = route }, Cmd.none )
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
                ( unitModel, cmd, maybeRoute, waitingServerResponse ) =
                    Units.Update.update msg model.unitModule
            in
                ( { model | unitModule = unitModel, redirectAfterServerResponse = maybeRoute, waitingServerResponse = waitingServerResponse }
                , Cmd.map Msgs.UnitMsg cmd
                )

        Msgs.MaterialMsg msg ->
            let
                ( materialModel, cmd, maybeRoute, waitingServerResponse ) =
                    Materials.Update.update msg model.materialModule
            in
                ( { model | materialModule = materialModel, redirectAfterServerResponse = maybeRoute, waitingServerResponse = waitingServerResponse }
                , Cmd.map Msgs.MaterialMsg cmd
                )

        Msgs.MessageMsg msg ->
            let
                ( messageModel, cmd ) =
                    MessageModule.update msg model.message
            in
                ( { model | message = messageModel }
                , Cmd.map Msgs.MessageMsg cmd
                )

        Msgs.StoreMsg msg ->
            let
                ( storeModel, cmd, message ) =
                    Store.Update.update msg model.store

                resultCmd =
                    case model.redirectAfterServerResponse of
                        Nothing ->
                            Cmd.map Msgs.StoreMsg cmd

                        Just route ->
                            Cmd.batch [ Cmd.map Msgs.StoreMsg cmd, Navigation.newUrl <| routeToHash route ]

                resultMessage =
                    if model.waitingServerResponse then
                        message
                    else
                        initMessage
            in
                ( { model | store = storeModel, message = resultMessage, redirectAfterServerResponse = Nothing, waitingServerResponse = False }
                , resultCmd
                )
