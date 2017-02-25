port module Unit exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Navigation
import Aliases exposing (Unit, initUnit, Message, initMessage, successMessage)
import Routes exposing (..)


-- model
--form model


type alias Model =
    { name : String
    , nameError : Maybe String
    , initials : String
    , initialsError : Maybe String
    , unit : Unit
    , query : String
    , error : Maybe String
    , displayingConfirmDialog : Bool
    , redirectRoute : Route
    }


type Subpage
    = IndexPage (List Unit)
    | NewPage
    | EditPage Unit
    | ShowPage Unit


subpageToHash : Subpage -> String
subpageToHash subpage =
    case subpage of
        IndexPage units ->
            routeToHash UnitIndexRoute

        NewPage ->
            routeToHash UnitNewRoute

        ShowPage unit ->
            routeToHash (UnitShowRoute unit.id)

        EditPage unit ->
            routeToHash (UnitEditRoute unit.id)


initModel : Model
initModel =
    { name = ""
    , nameError = Nothing
    , initials = ""
    , initialsError = Nothing
    , unit = initUnit
    , query = ""
    , error = Nothing
    , displayingConfirmDialog = False
    , redirectRoute = UnitIndexRoute
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = PrepareView Subpage Message
    | NavigateRoute Route
    | NameInputChanged String
    | InitialsInputChanged String
    | Add Unit
    | Added Unit
    | Update Unit
    | Updated Unit
    | Remove Unit
    | Removed String


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        PrepareView subpage message ->
            case subpage of
                EditPage unit ->
                    ( { model | name = unit.name, initials = unit.initials, unit = unit }
                    , Cmd.none
                    , initMessage
                    )

                _ ->
                    ( { model | unit = initUnit, name = initUnit.name, initials = initUnit.initials }
                    , Cmd.none
                    , initMessage
                    )

        NavigateRoute route ->
            ( model
            , Navigation.newUrl <| routeToHash route
            , initMessage
            )

        NameInputChanged name_ ->
            let
                actualModel =
                    model.unit

                updatedModel =
                    { actualModel | name = name_ }
            in
                ( { model
                    | name = name_
                    , nameError = Nothing
                    , unit = updatedModel
                  }
                , Cmd.none
                , initMessage
                )

        InitialsInputChanged initials_ ->
            let
                actualModel =
                    model.unit

                updatedModel =
                    { actualModel | initials = initials_ }
            in
                ( { model
                    | initials = initials_
                    , initialsError = Nothing
                    , unit = updatedModel
                  }
                , Cmd.none
                , initMessage
                )

        Add unit ->
            ( model, addUnit unit, initMessage )

        Added unit ->
            ( { model | unit = initUnit }, Navigation.newUrl (routeToHash model.redirectRoute), successMessage "Unit successfully added" )

        Remove unit ->
            ( model, removeUnit unit, initMessage )

        Removed id ->
            ( model, Cmd.none, successMessage "Unit successfully removed" )

        Update unit ->
            ( model, updateUnit unit, initMessage )

        Updated updatedUnit ->
            ( { model | unit = initUnit }, Navigation.newUrl (routeToHash model.redirectRoute), successMessage "Unit successfully updated" )


view : Model -> Subpage -> Html Msg
view model subpage =
    let
        page =
            case subpage of
                IndexPage units ->
                    unitsToTable units model

                NewPage ->
                    unitForm model Nothing

                EditPage unit ->
                    unitForm model (Just unit)

                ShowPage unit ->
                    unitShow unit
    in
        div [ class "main" ]
            [ errorPanel model.error
            , page
            , confirmModalView model
            ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                , button [ type_ "button" ] [ text "×" ]
                ]


unitsToTable : List Unit -> Model -> Html Msg
unitsToTable units model =
    -- filter using query afterwards
    let
        table_ =
            List.map (unitToTr) units
                |> tbody []
                |> (\r -> unitsTh :: [ r ])
                |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Units list" ]
            , table_
            , button [ class "ui button", onClick (NavigateRoute (UnitNewRoute)) ] [ text "New" ]
            ]


unitsTh : Html Msg
unitsTh =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Initials" ]
            , th [] [ text "Actions" ]
            ]
        ]


unitToTr : Unit -> Html Msg
unitToTr unit =
    tr []
        [ td [] [ text unit.id ]
        , td [] [ text unit.name ]
        , td [] [ text unit.initials ]
        , td []
            [ button [ class "ui button", onClick (NavigateRoute (UnitEditRoute unit.id)) ] [ text "Edit" ]
            , button [ class "ui button", onClick (NavigateRoute (UnitShowRoute unit.id)) ] [ text "Show" ]
            , button [ class "ui button", onClick (Remove unit) ] [ text "Remove" ]
            ]
        ]


unitShow : Unit -> Html Msg
unitShow unit =
    div [ class "ui items" ]
        [ div [ class "item" ]
            [ div [ class "content" ]
                [ a [ class "header" ] [ text ("Showing unit " ++ unit.name) ]
                , div [ class "meta" ]
                    [ span [] [ text ("Identifier: " ++ unit.id) ] ]
                , div [ class "meta" ]
                    [ span [] [ text ("Name: " ++ unit.name) ] ]
                , div [ class "meta" ]
                    [ span [] [ text ("Initials: " ++ unit.initials) ] ]
                ]
            ]
        , button [ class "ui button", onClick (NavigateRoute (UnitEditRoute unit.id)) ] [ text "Edit" ]
        , button [ class "ui button red", onClick (Remove unit) ] [ text "Remove" ]
        , button [ class "ui button", onClick (NavigateRoute UnitIndexRoute) ] [ text "List Units" ]
        ]


confirmModalView : Model -> Html Msg
confirmModalView model =
    div [ class "ui modal hidden" ]
        [ div [ class "ui header" ] [ text "Are you sure?" ]
        , div [ class "actions" ]
            [ div [ class "ui red cancel button" ] [ text "Nope" ]
            , div [ class "ui green ok button" ] [ text "Yep" ]
            ]
        ]


unitForm : Model -> Maybe Unit -> Html Msg
unitForm model maybeUnit =
    let
        ( action, headerMessage, buttons ) =
            let
                sureButtons =
                    [ button [ type_ "submit", class "ui blue submit button" ] [ text "Save" ]
                    , a [ class "ui button", onClick (NavigateRoute UnitIndexRoute) ] [ text "List Units" ]
                    ]

                maybeButton =
                    [ a [ class "ui button", onClick (NavigateRoute (UnitShowRoute (Maybe.withDefault initUnit maybeUnit).id)) ] [ text "Show" ] ]
            in
                if maybeUnit == Nothing then
                    ( Add model.unit, "New unit", sureButtons )
                else
                    ( Update model.unit, "Editing unit with id: " ++ (Maybe.withDefault initUnit maybeUnit).id, (List.append sureButtons maybeButton) )
    in
        Html.form [ class "ui large form", onSubmit action ]
            [ div [ class "ui stacked segment" ]
                [ h1 [ class "ui header" ] [ text headerMessage ]
                , div [ class "field" ]
                    [ label [] [ text "Name" ]
                    , input
                        [ type_ "text"
                        , value model.name
                        , onInput NameInputChanged
                        ]
                        []
                    , span [] [ text <| Maybe.withDefault "" model.nameError ]
                    ]
                , div [ class "field" ]
                    [ label [] [ text "Initials" ]
                    , input
                        [ type_ "text"
                        , value model.initials
                        , onInput InitialsInputChanged
                        ]
                        []
                    , span [] [ text <| Maybe.withDefault "" model.initialsError ]
                    ]
                , div [] buttons
                ]
            ]


port addUnit : Unit -> Cmd msg


port unitAdded : (Unit -> msg) -> Sub msg


port updateUnit : Unit -> Cmd msg


port unitUpdated : (Unit -> msg) -> Sub msg


port removeUnit : Unit -> Cmd msg


port unitRemoved : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ unitAdded Added
        , unitUpdated Updated
        , unitRemoved Removed
        ]
