port module Materials.Main exposing (..)

import Materials.Model exposing (Model, initModel)
import Materials.Msgs exposing (Msg)


-- model
--form models


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )
