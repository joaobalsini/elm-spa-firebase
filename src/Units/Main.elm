port module Units.Main exposing (..)

import Units.Model exposing (Model, initModel)
import Units.Msgs exposing (Msg)


-- model
--form models


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )
