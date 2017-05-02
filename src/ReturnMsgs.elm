module ReturnMsgs exposing (..)

import Routes exposing (Route)
import Materials.Model


type ReturnMsg
    = NoOp
    | ShowNotification String
    | WaitForServerSuccessAndRedirectToRoute Route
    | WaitForServerSuccessAndRedirectWithDefaultRouteAndNotification Route String
    | WaitForServerSuccessAndRedirectToRouteWithNotification Route String
    | WaitForServerSuccessAndShowNotification String
    | SaveForLaterWaitForServerSuccessAndRedirectToRoute Route
    | SaveForLaterWaitForServerSuccessAndRedirectToRouteWithNotification Route String
    | SaveForLaterWaitForServerSuccessAndRedirectToRouteWithNotificationRestoringMaterialModel Route String Materials.Model.Model
