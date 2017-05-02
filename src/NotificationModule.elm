port module NotificationModule exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type alias Notification =
    { notificationClass : String
    , header : String
    , text : String
    , active : Bool
    }


initNotification : Notification
initNotification =
    { notificationClass = ""
    , header = ""
    , text = ""
    , active = False
    }


errorNotification : String -> Notification
errorNotification text =
    { initNotification
        | notificationClass = "negative"
        , header = "Error"
        , text = text
        , active = True
    }


successNotification : String -> Notification
successNotification text =
    { initNotification
        | notificationClass = "positive"
        , header = "Success"
        , text = text
        , active = True
    }


init : ( Notification, Cmd Msg )
init =
    ( initNotification, Cmd.none )


type Msg
    = Show Notification
    | Hide


update : Msg -> Notification -> ( Notification, Cmd Msg )
update msg model =
    case msg of
        Show notification ->
            ( { model | notificationClass = notification.notificationClass, header = notification.header, text = notification.text, active = True }
            , Cmd.none
            )

        Hide ->
            ( { model | active = False }
            , Cmd.none
            )


view : Notification -> Html Msg
view model =
    let
        class_ =
            "ui message " ++ model.notificationClass

        render =
            if model.active == False then
                div [] []
            else
                div [ class class_ ]
                    [ i [ class "close icon", onClick Hide ] []
                    , div [ class "header" ] [ text model.header ]
                    , p [] [ text model.text ]
                    ]
    in
        render


port portNotification : (Notification -> msg) -> Sub msg


subscriptions : Notification -> Sub Msg
subscriptions model =
    Sub.batch
        [ portNotification Show
        ]
