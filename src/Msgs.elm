module Msgs exposing (..)

import Routes exposing (..)
import Materials.Msgs
import Units.Msgs
import IndexModule
import LoginModule
import NotificationModule
import Store.Msgs
import Materials.Model


type Msg
    = Navigate Route
    | ChangePage Route
    | IndexMsg IndexModule.Msg
    | LoginMsg LoginModule.Msg
    | UnitMsg Units.Msgs.Msg
    | MaterialMsg Materials.Msgs.Msg
    | NotificationMsg NotificationModule.Msg
    | StoreMsg Store.Msgs.Msg


type ReturnMsg
    = NoOp
    | ShowNotification String
    | WaitForServerSuccessAndRedirectToRoute Route
    | WaitForServerSuccessAndRedirectWithDefaultRouteAndNotification Route String
    | WaitForServerSuccessAndRedirectToRouteWithNotification Route String
    | WaitForServerSuccessAndShowNotification String
    | WaitForServerSuccessAndRedirectToRouteWithNotificationRestoringMaterialModel Route String Materials.Model.Model
