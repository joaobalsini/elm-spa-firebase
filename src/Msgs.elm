module Msgs exposing (..)

import Routes exposing (..)
import Materials.Msgs
import Units.Msgs
import IndexModule
import LoginModule
import MessageModule
import Store.Msgs


type Msg
    = Navigate Route
    | ChangePage Route
    | IndexMsg IndexModule.Msg
    | LoginMsg LoginModule.Msg
    | UnitMsg Units.Msgs.Msg
    | MaterialMsg Materials.Msgs.Msg
    | MessageMsg MessageModule.Msg
    | StoreMsg Store.Msgs.Msg
