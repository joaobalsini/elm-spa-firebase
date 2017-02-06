port module Message exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { messageClass : String
    , header : String
    , text : String
    , active : Bool
    }


initModel : Model
initModel =
    { messageClass = ""
    , header = ""
    , text = ""
    , active = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type Msg
    = Show Model
    | Hide


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Show message ->
            ( { model | messageClass = message.messageClass, header = message.header, text = message.text, active = True }
            , Cmd.none
            )

        Hide ->
            ( { model | active = False }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        class_ =
            "ui message " ++ model.messageClass

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


port errorMessage : (Model -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ errorMessage Show
        ]
