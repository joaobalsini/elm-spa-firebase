port module Store.Subscriptions exposing (..)

import Store.Model exposing (Model)
import Store.Msgs exposing (..)
import Units.Model exposing (UnitDB)
import Materials.Model exposing (MaterialDB)


port materialAdded : (MaterialDB -> msg) -> Sub msg


port materialUpdated : (MaterialDB -> msg) -> Sub msg


port materialRemoved : (String -> msg) -> Sub msg


port unitAdded : (UnitDB -> msg) -> Sub msg


port unitUpdated : (UnitDB -> msg) -> Sub msg


port unitRemoved : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ materialAdded MaterialAdded
        , materialUpdated MaterialUpdated
        , materialRemoved MaterialRemoved
        , unitAdded UnitAdded
        , unitUpdated UnitUpdated
        , unitRemoved UnitRemoved
        ]
