module Routing exposing (..)

import Html exposing (Html, a, button, code, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Unit
import Material
import Index
import Login


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { route : Route }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            parseLocation location

        initModel =
            { route = route }
    in
        ( initModel
        , Cmd.none
        )



-- URL PARSING


type Route
    = IndexRoute
    | LoginRoute
    | UnitIndexRoute
    | UnitShowRoute String
    | UnitNewRoute
    | UnitEditRoute String
    | MaterialIndexRoute
    | MaterialShowRoute String
    | MaterialNewRoute
    | MaterialEditRoute String
    | NotFoundRoute


matchers : Url.Parser (Route -> a) a
matchers =
    Url.oneOf
        [ Url.map IndexRoute top
        , Url.map UnitEditRoute (s "units/edit" </> Url.string)
        , Url.map UnitNewRoute (s "units/new")
        , Url.map UnitShowRoute (s "units" </> Url.string)
        , Url.map UnitIndexRoute (s "units")
        , Url.map MaterialEditRoute (s "materials/edit" </> Url.string)
        , Url.map MaterialNewRoute (s "materials/new")
        , Url.map MaterialShowRoute (s "materials" </> Url.string)
        , Url.map MaterialIndexRoute (s "materials")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (Url.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



-- UPDATE


type Msg
    = NewUrl String
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model
            , Navigation.newUrl url
            )

        UrlChange location ->
            let
                route =
                    parseLocation location
            in
                ( { model | route = route }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Links" ]
        , ul [] (List.map viewLink [ "/", "/blog/", "/blog/42", "/blog/37", "/blog/?search=cats" ])
        ]


viewLink : String -> Html Msg
viewLink url =
    li [] [ button [ onClick (NewUrl url) ] [ text url ] ]
