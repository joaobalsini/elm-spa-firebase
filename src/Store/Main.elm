module Store.Main exposing (..)

import Store.Model exposing (Model, initModel)
import Store.Msgs exposing (Msg)


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )
