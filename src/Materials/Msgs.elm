module Materials.Msgs exposing (..)

import Routes exposing (Route)
import Materials.Model exposing (Material, MaterialDB)
import ReturnMsgs exposing (ReturnMsg)


type Msg
    = LoadFormData Material
    | ClearFormData
    | NavigateRoute Route ReturnMsg
    | RedirectBack
    | NameInputChanged String
    | UnitSelectChanged String
    | InventoryInputChanged String
    | SubmitMaterialForm (Maybe Material)
    | RequestRemoveConfirmation String
    | CancelRemoveConfirmation
    | Remove String
