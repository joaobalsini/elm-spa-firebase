port module Unit exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Navigation
import Aliases exposing (Unit, initUnit, Message, initMessage)


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
    }


type Subpage
    = IndexPage
    | NewPage
    | EditPage String
    | ShowPage String


subpageToHash : Subpage -> String
subpageToHash subpage =
    case subpage of
        IndexPage ->
            "#/units"

        NewPage ->
            "#/units/new"

        ShowPage id ->
            "#/units/" ++ id

        EditPage id ->
            "#/units/edit/" ++ id



-- real model


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
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = Navigate Subpage (List Unit) Message
    | NameInputChanged String
    | InitialsInputChanged String
    | Add Unit
    | Added Unit
    | Update Unit
    | Updated Unit
    | Remove Unit
    | Removed String


getUnitByIdFromUnitsList : List Unit -> String -> Maybe Unit
getUnitByIdFromUnitsList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getUnitByIdFromUnitsList xs id


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        Navigate subpage units message ->
            case subpage of
                ShowPage id ->
                    let
                        -- if unit is Nothing, redirect back and show error
                        unit =
                            getUnitByIdFromUnitsList units id

                        -- if unit is Nothing, redirect back and show error
                        unitOrClearUnit =
                            Maybe.withDefault initUnit unit
                    in
                        ( { model | unit = unitOrClearUnit }
                        , Navigation.newUrl <| subpageToHash subpage
                        , initMessage
                        )

                EditPage id ->
                    let
                        unit =
                            getUnitByIdFromUnitsList units id

                        -- if unit is Nothing, redirect back and show error
                        unitOrClearUnit =
                            Maybe.withDefault initUnit unit
                    in
                        ( { model | name = unitOrClearUnit.name, initials = unitOrClearUnit.initials, unit = unitOrClearUnit }
                        , Navigation.newUrl <| subpageToHash subpage
                        , initMessage
                        )

                _ ->
                    ( { model | unit = initUnit, name = "", initials = "" }
                    , Navigation.newUrl <| subpageToHash subpage
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
            let
                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess added"
                        , text = "Unit successfully added"
                        , active = True
                    }
            in
                ( { model | unit = initUnit }, Navigation.newUrl "#/units", message )

        Remove unit ->
            ( model, removeUnit unit, initMessage )

        Removed id ->
            let
                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess removed"
                        , text = "Unit successfully removed"
                        , active = True
                    }
            in
                ( model, Cmd.none, message )

        Update unit ->
            ( model, updateUnit unit, initMessage )

        Updated updatedUnit ->
            let
                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess updated"
                        , text = "Unit successfully updated"
                        , active = True
                    }
            in
                ( { model | unit = initUnit }, Navigation.newUrl "#/units", message )


view : Model -> List Unit -> Subpage -> Html Msg
view model units subpage =
    let
        page =
            case subpage of
                IndexPage ->
                    unitsToTable units model

                NewPage ->
                    unitForm model units Nothing

                EditPage string ->
                    unitForm model units (Just string)

                ShowPage string ->
                    unitShow model units string
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
            List.map (unitToTr units) units
                |> tbody []
                |> (\r -> unitsTh :: [ r ])
                |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Units list" ]
            , table_
            , button [ class "ui button", onClick (Navigate NewPage units initMessage) ] [ text "New" ]
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


unitToTr : List Unit -> Unit -> Html Msg
unitToTr units unit =
    tr []
        [ td [] [ text unit.id ]
        , td [] [ text unit.name ]
        , td [] [ text unit.initials ]
        , td []
            [ button [ class "ui button", onClick (Navigate (EditPage unit.id) units initMessage) ] [ text "Edit" ]
            , button [ class "ui button", onClick (Navigate (ShowPage unit.id) units initMessage) ] [ text "Show" ]
            , button [ class "ui button", onClick (Remove unit) ] [ text "Remove" ]
            ]
        ]


unitShow : Model -> List Unit -> String -> Html Msg
unitShow model units key =
    div [ class "ui items" ]
        [ div [ class "item" ]
            [ div [ class "content" ]
                [ a [ class "header" ] [ text ("Showing unit " ++ model.unit.name) ]
                , div [ class "meta" ]
                    [ span [] [ text ("Identifier: " ++ model.unit.id) ] ]
                , div [ class "meta" ]
                    [ span [] [ text ("Name: " ++ model.unit.name) ] ]
                , div [ class "meta" ]
                    [ span [] [ text ("Initials: " ++ model.unit.initials) ] ]
                ]
            ]
        , button [ class "ui button", onClick (Navigate (EditPage model.unit.id) units initMessage) ] [ text "Edit" ]
        , button [ class "ui button red", onClick (Remove model.unit) ] [ text "Remove" ]
        , button [ class "ui button", onClick (Navigate IndexPage units initMessage) ] [ text "List Units" ]
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


unitForm : Model -> List Unit -> Maybe String -> Html Msg
unitForm model units maybeKey =
    let
        ( action, headerMessage, buttons ) =
            let
                sureButtons =
                    [ button [ type_ "submit", class "ui blue submit button" ] [ text "Save" ]
                    , button [ class "ui button", onClick (Navigate IndexPage units initMessage) ] [ text "List Units" ]
                    ]

                maybeButton =
                    [ button [ class "ui button", onClick (Navigate (ShowPage model.unit.id) units initMessage) ] [ text "Show" ] ]
            in
                if maybeKey == Nothing then
                    ( Add model.unit, "New unit", sureButtons )
                else
                    ( Update model.unit, "Editing unit with id: " ++ (Maybe.withDefault "" maybeKey), (List.append sureButtons maybeButton) )
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
