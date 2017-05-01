port module Subscriptions exposing (..)

import Models exposing (Model)
import Msgs exposing (Msg)
import IndexModule
import LoginModule
import NotificationModule
import Store.Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        loginSub =
            LoginModule.subscriptions model.loginModule

        indexSub =
            IndexModule.subscriptions model.indexModule

        storeSub =
            Store.Subscriptions.subscriptions model.store

        notificationSub =
            NotificationModule.subscriptions model.notification
    in
        Sub.batch
            [ Sub.map Msgs.IndexMsg indexSub
            , Sub.map Msgs.LoginMsg loginSub
            , Sub.map Msgs.StoreMsg storeSub
            , Sub.map Msgs.NotificationMsg notificationSub
            ]
