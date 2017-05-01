module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Routes exposing (..)
import Materials.Routes
import Units.Routes
import Msgs exposing (Msg)
import Models exposing (Model)
import Materials.View
import Units.View
import IndexModule
import LoginModule
import NotificationModule


view : Model -> Html Msg
view model =
    let
        -- get the page through the view method of each Module passing the parameters needed and render that page
        page =
            if model.waitingServerResponse == True then
                div [ class "ui active inverted dimmer" ]
                    [ div [ class "ui text loader" ] [ text "Waiting server..." ]
                    ]
            else
                case model.route of
                    IndexRoute ->
                        Html.map Msgs.IndexMsg
                            (IndexModule.view model.indexModule)

                    LoginRoute ->
                        Html.map Msgs.LoginMsg
                            (LoginModule.view model.loginModule)

                    UnitsRoutes subroute ->
                        Html.map Msgs.UnitMsg (Units.View.view model.unitModule model.store.units subroute)

                    MaterialsRoutes subroute ->
                        Html.map Msgs.MaterialMsg (Materials.View.view model.materialModule model.store.materials model.store.units subroute)

                    NotFoundRoute ->
                        div [ class "main" ]
                            [ h1 []
                                [ text "Page Not Found!" ]
                            ]
    in
        div []
            [ div [ class "ui fixed inverted menu" ] [ pageHeader model ]
            , Html.map Msgs.NotificationMsg (NotificationModule.view model.notification)
            , div [ class "ui main text container" ] [ page ]
            ]


pageHeader : Model -> Html Msg
pageHeader model =
    div [ class "ui container" ]
        [ a [ class "item", onClick (Msgs.Navigate IndexRoute) ] [ text "Index" ]
        , a [ class "item", onClick (Msgs.Navigate (MaterialsRoutes Materials.Routes.MaterialIndexRoute)) ] [ text "Materials" ]
        , a [ class "item", onClick (Msgs.Navigate (UnitsRoutes Units.Routes.UnitIndexRoute)) ] [ text "Units" ]
        , a [ class "item right", onClick (Msgs.Navigate LoginRoute) ] [ text "Login" ]
        ]
