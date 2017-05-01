module Units.Msgs exposing (..)

import Routes exposing (Route)
import Units.Model exposing (Unit, UnitDB)


type Msg
    = LoadFormData Unit
    | ClearFormData
    | NavigateRoute Route
    | RedirectBack
    | NameInputChanged String
    | InitialsInputChanged String
    | SubmitUnitForm (Maybe Unit)
    | RequestRemoveConfirmation String
    | CancelRemoveConfirmation
    | Remove String
