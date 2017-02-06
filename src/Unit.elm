port module Unit exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Navigation
import Message


-- model
--form model


type alias Model =
    { id : String
    , name : String
    , nameError : Maybe String
    , initials : String
    , initialsError : Maybe String
    , unit : Unit
    , units : List Unit
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


type alias Unit =
    { id : String
    , name : String
    , initials : String
    }


initUnit : Unit
initUnit =
    { id = ""
    , name = ""
    , initials = ""
    }


initModel : Model
initModel =
    { id = ""
    , name = ""
    , nameError = Nothing
    , initials = ""
    , initialsError = Nothing
    , unit = initUnit
    , units = []
    , query = ""
    , error = Nothing
    , displayingConfirmDialog = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = Navigate Subpage Message.Model
    | NameInputChanged String
    | InitialsInputChanged String
    | Add Unit
    | Added Unit
    | Update Unit
    | Updated Unit
    | Remove Unit
    | Removed String


updateUnitAtId : Unit -> String -> Unit -> Unit
updateUnitAtId updatedUnit unitId originalUnit =
    if originalUnit.id == unitId then
        { originalUnit | name = updatedUnit.name, initials = updatedUnit.initials }
    else
        originalUnit


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


update : Msg -> Model -> ( Model, Cmd Msg, Message.Model )
update msg model =
    case msg of
        Navigate subpage message ->
            case subpage of
                ShowPage id ->
                    ( model
                    , Navigation.newUrl <| subpageToHash subpage
                    , Message.initModel
                    )

                EditPage id ->
                    let
                        unit =
                            getUnitByIdFromUnitsList model.units id

                        unitOrClearUnit =
                            Maybe.withDefault initUnit unit
                    in
                        ( { model | id = id, name = unitOrClearUnit.name, initials = unitOrClearUnit.initials, unit = unitOrClearUnit }
                        , Navigation.newUrl <| subpageToHash subpage
                        , Message.initModel
                        )

                _ ->
                    ( { model | unit = initUnit, name = "", initials = "" }
                    , Navigation.newUrl <| subpageToHash subpage
                    , Message.initModel
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
                , Message.initModel
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
                , Message.initModel
                )

        Add unit ->
            ( model, addUnit unit, Message.initModel )

        Added unit ->
            let
                newUnits =
                    unit :: model.units

                initMessage =
                    Message.initModel

                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess added"
                        , text = "Unit successfully added"
                        , active = True
                    }
            in
                ( { model | units = newUnits, unit = initUnit }, Navigation.newUrl "#/units", message )

        Remove unit ->
            ( model, removeUnit unit, Message.initModel )

        Removed id ->
            let
                newUnits =
                    List.filter (\unit -> unit.id /= id)
                        model.units

                initMessage =
                    Message.initModel

                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess removed"
                        , text = "Unit successfully removed"
                        , active = True
                    }
            in
                ( { model | units = newUnits }, Cmd.none, message )

        Update unit ->
            ( model, updateUnit unit, Message.initModel )

        Updated updatedUnit ->
            -- create update unit
            let
                -- map puts the element as last parameter for function updateUnitIfIdMatches
                newUnits =
                    List.map (updateUnitAtId updatedUnit updatedUnit.id) model.units

                initMessage =
                    Message.initModel

                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess updated"
                        , text = "Unit successfully updated"
                        , active = True
                    }

                -- this function updates a unit element IF the id matches
            in
                ( { model | units = newUnits, unit = initUnit }, Navigation.newUrl "#/units", message )


view : Model -> Subpage -> Html Msg
view model subpage =
    div [ class "main" ]
        [ errorPanel model.error
        , renderSubPage model subpage
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


renderSubPage : Model -> Subpage -> Html Msg
renderSubPage model subpage =
    case subpage of
        IndexPage ->
            listView model

        NewPage ->
            formView model Nothing

        EditPage string ->
            formView model (Just string)

        ShowPage string ->
            showView model string



-- type Subpage
--     = UnitIndexPage
--     | UnitNewPage
--     | UnitEditPage Int
--     | UnitShowPage Int


listView : Model -> Html Msg
listView { query, units } =
    -- filter using query afterwards
    let
        table_ =
            units
                |> List.map unitView
                |> tbody []
                |> (\r -> unitsHeader :: [ r ])
                |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Units list" ]
            , table_
            , button [ class "ui button", onClick (Navigate NewPage Message.initModel) ] [ text "New" ]
            ]


unitsHeader : Html Msg
unitsHeader =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Initials" ]
            , th [] [ text "Actions" ]
            ]
        ]


unitView : Unit -> Html Msg
unitView unit =
    tr []
        [ td [] [ text unit.id ]
        , td [] [ text unit.name ]
        , td [] [ text unit.initials ]
        , td []
            [ button [ class "ui button", onClick (Navigate (EditPage unit.id) Message.initModel) ] [ text "Edit" ]
            , button [ class "ui button", onClick (Navigate (ShowPage unit.id) Message.initModel) ] [ text "Show" ]
            , button [ class "ui button", onClick (Remove unit) ] [ text "Remove" ]
            ]
        ]


showView : Model -> String -> Html Msg
showView model key =
    div [] [ text ("Showing unit id:" ++ key) ]


confirmModalView : Model -> Html Msg
confirmModalView model =
    div [ class "ui modal hidden" ]
        [ div [ class "ui header" ] [ text "Are you sure?" ]
        , div [ class "actions" ]
            [ div [ class "ui red cancel button" ] [ text "Nope" ]
            , div [ class "ui green ok button" ] [ text "Yep" ]
            ]
        ]


formView : Model -> Maybe String -> Html Msg
formView model maybeKey =
    let
        ( action, headerMessage ) =
            if maybeKey == Nothing then
                ( Add model.unit, "New unit" )
            else
                ( Update model.unit, "Editing unit with id: " ++ (Maybe.withDefault "" maybeKey) )
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
                , div []
                    [ label [] []
                    , button [ type_ "submit", class "ui fluid large teal submit button" ] [ text "Save" ]
                    ]
                ]
            ]


port addUnit : Unit -> Cmd msg


port unitAdded : (Unit -> msg) -> Sub msg


port updateUnit : Unit -> Cmd msg


port unitUpdated : (Unit -> msg) -> Sub msg


port removeUnit : Unit -> Cmd msg


port unitRemoved : (String -> msg) -> Sub msg


port loadUnit : String -> Cmd msg


port unitLoaded : (Unit -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ unitAdded Added
        , unitUpdated Updated
        , unitRemoved Removed
        ]
