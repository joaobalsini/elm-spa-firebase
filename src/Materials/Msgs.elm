module Materials.Msgs exposing (..)

import Routes exposing (Route)
import Materials.Model exposing (Material, MaterialDB)


type Msg
    = LoadFormData Material
    | ClearFormData
    | NavigateRoute Route
    | RedirectBack
    | NameInputChanged String
    | UnitSelectChanged String
    | InventoryInputChanged String
    | SubmitMaterialForm String
    | RequestRemoveConfirmation String
    | CancelRemoveConfirmation
    | Remove String
