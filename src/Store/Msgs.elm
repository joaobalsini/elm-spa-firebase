module Store.Msgs exposing (..)

import Units.Model exposing (UnitDB)
import Materials.Model exposing (MaterialDB)


type Msg
    = LoadMaterials
    | MaterialAdded MaterialDB
    | MaterialUpdated MaterialDB
    | MaterialRemoved String
    | LoadUnits
    | UnitAdded UnitDB
    | UnitUpdated UnitDB
    | UnitRemoved String
