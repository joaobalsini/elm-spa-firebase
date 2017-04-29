port module Store.Commands exposing (..)

import Materials.Model exposing (MaterialDB)
import Units.Model exposing (UnitDB)


port loadMaterials : () -> Cmd msg


port addMaterial : MaterialDB -> Cmd msg


port updateMaterial : MaterialDB -> Cmd msg


port removeMaterial : String -> Cmd msg


port loadUnits : () -> Cmd msg


port addUnit : UnitDB -> Cmd msg


port updateUnit : UnitDB -> Cmd msg


port removeUnit : String -> Cmd msg
