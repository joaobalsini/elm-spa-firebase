module Msgs exposing (..)

import Routes exposing (..)
import Materials.Msgs
import Units.Msgs
import IndexModule
import LoginModule
import MessageModule
import Store.Msgs
import Materials.Model


type Msg
    = Navigate Route
    | ChangePage Route
    | IndexMsg IndexModule.Msg
    | LoginMsg LoginModule.Msg
    | UnitMsg Units.Msgs.Msg
    | MaterialMsg Materials.Msgs.Msg
    | MessageMsg MessageModule.Msg
    | StoreMsg Store.Msgs.Msg


type ReturnMsg
    = NoOp
    | ShowMessage String
    | WaitForServerSuccessAndRedirectToRoute Route
    | WaitForServerSuccessAndRedirectWithDefaultRouteAndMessage Route String
    | WaitForServerSuccessAndRedirectToRouteWithMessage Route String
    | WaitForServerSuccessAndShowMessage String
    | WaitForServerSuccessAndRedirectToRouteWithMessageRestoringMaterialModel Route String Materials.Model.Model
