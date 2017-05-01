module Models exposing (..)

import Routes exposing (..)
import Msgs exposing (Msg, ReturnMsg)
import Materials.Main
import Materials.Model
import Units.Main
import Units.Model
import Store.Main
import Store.Model
import IndexModule
import LoginModule
import MessageModule
import Navigation


type alias Model =
    { route : Route
    , lastRoute : Route
    , loginModule : LoginModule.Model
    , indexModule : IndexModule.Model
    , unitModule : Units.Model.Model
    , message : MessageModule.Message
    , materialModule : Materials.Model.Model
    , store : Store.Model.Model
    , returnMsgsToProcess : List ReturnMsg
    , waitingServerResponse : Bool
    }


initModelandCmds : Route -> ( Model, Cmd Msg )
initModelandCmds route =
    let
        ( indexInitModel, indexCmd ) =
            IndexModule.init

        ( loginInitModel, loginCmd ) =
            LoginModule.init

        ( unitInitModel, unitCmd ) =
            Units.Main.init

        ( materialInitModel, materialCmd ) =
            Materials.Main.init

        ( messageInitModel, messageCmd ) =
            MessageModule.init

        ( storeInitModel, storeCmd ) =
            Store.Main.init

        cmds =
            Cmd.batch
                [ Navigation.newUrl <| routeToHash route
                , Cmd.map Msgs.LoginMsg loginCmd
                , Cmd.map Msgs.IndexMsg indexCmd
                , Cmd.map Msgs.UnitMsg unitCmd
                , Cmd.map Msgs.MaterialMsg materialCmd
                , Cmd.map Msgs.MessageMsg messageCmd
                , Cmd.map Msgs.StoreMsg storeCmd
                ]
    in
        ( { route = route
          , lastRoute = route
          , loginModule = loginInitModel
          , indexModule = indexInitModel
          , message = MessageModule.initMessage
          , unitModule = unitInitModel
          , materialModule = materialInitModel
          , store = storeInitModel
          , returnMsgsToProcess = []
          , waitingServerResponse = False
          }
        , cmds
        )
