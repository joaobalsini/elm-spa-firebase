port module IndexModule exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- model


type alias Model =
    { notification : Maybe String
    , query : String
    }


initModel : Model
initModel =
    { notification = Nothing
    , query = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = SearchInput String
    | Search


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )

        Search ->
            ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ h1 [ class "ui header" ] [ text "Initial Page - Index" ]
        , notificationPanel model.notification
        , searchForm model.query
        ]


notificationPanel : Maybe String -> Html a
notificationPanel notification =
    case notification of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                , button [ type_ "button" ] [ text "×" ]
                ]


searchForm : String -> Html Msg
searchForm query =
    Html.form [ onSubmit Search ]
        [ input
            [ type_ "text"
            , placeholder "Search..."
            , value query
            , onInput SearchInput
            ]
            []
        , button [ type_ "submit" ] [ text "Search" ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
