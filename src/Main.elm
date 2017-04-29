port module Main exposing (..)

import Navigation exposing (Location)
import Routes exposing (..)
import Update exposing (update)
import Msgs exposing (Msg)
import Models exposing (Model, initModelandCmds)
import Subscriptions exposing (subscriptions)
import View exposing (view)


main : Program Never Model Msg
main =
    Navigation.program locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- model


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            locationToRoute location
    in
        initModelandCmds route



-- this function is triggered whenever the user changes the url


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location
        |> locationToRoute
        |> Msgs.ChangePage
