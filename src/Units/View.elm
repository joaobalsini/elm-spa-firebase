module Units.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Units.Model exposing (..)
import Units.Msgs exposing (..)
import Routes exposing (..)
import Units.Routes exposing (..)
import Utils.CommonDefinitions exposing (FieldError)


view : Model -> Maybe (List Unit) -> UnitRoute -> Html Msg
view model maybeUnits subroute =
    case maybeUnits of
        Nothing ->
            div [ class "ui active inverted dimmer" ]
                [ div [ class "ui text loader" ] [ text "Loading units" ]
                ]

        Just units ->
            let
                page =
                    case subroute of
                        UnitIndexRoute ->
                            unitsToTable units

                        UnitNewRoute ->
                            unitForm model Nothing

                        UnitEditRoute id ->
                            let
                                maybeUnit =
                                    Units.Model.getUnitByIdFromList units id
                            in
                                case maybeUnit of
                                    Nothing ->
                                        text ("Unit not found with id:" ++ id)

                                    Just unit ->
                                        unitForm model (Just id)

                        UnitShowRoute id ->
                            let
                                maybeUnit =
                                    Units.Model.getUnitByIdFromList units id
                            in
                                case maybeUnit of
                                    Nothing ->
                                        text ("Unit not found with id:" ++ id)

                                    Just unit ->
                                        unitShow unit
            in
                div [ class "main" ]
                    [ unitFormErrorPanel model
                    , page
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


unitOption : String -> Unit -> Html Msg
unitOption selectedUnitId unit =
    if unit.name == "" then
        option [ value "", selected (selectedUnitId == "") ] [ text "Please select one unit" ]
    else
        option [ value unit.id, selected (selectedUnitId == unit.id) ] [ text (unit.name ++ "(" ++ unit.initials ++ ")") ]


unitsToTable : List Unit -> Html Msg
unitsToTable units =
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
            , button [ class "ui button", onClick (NavigateRoute (UnitsRoutes UnitNewRoute)) ] [ text "New" ]
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
            [ button [ class "ui button", onClick (NavigateRoute (UnitsRoutes (UnitEditRoute unit.id))) ] [ text "Edit" ]
            , button [ class "ui button", onClick (NavigateRoute (UnitsRoutes (UnitShowRoute unit.id))) ] [ text "Show" ]
            , button [ class "ui button", onClick (Remove unit.id) ] [ text "Remove" ]
            ]
        ]


unitShow : Unit -> Html Msg
unitShow unit =
    div []
        [ div [ class "ui stacked segment" ]
            [ h1 [] [ text ("Showing unit id:" ++ unit.id) ]
            , h4 [] [ text "Name" ]
            , p [] [ text unit.name ]
            , h4 [] [ text "Initials" ]
            , p [] [ text unit.initials ]
            ]
        , a [ href "javascript:void(0);", onClick (RedirectBack) ] [ text "Back" ]
        , span [] [ text " | " ]
        , a [ href "javascript:void(0);", onClick (NavigateRoute (UnitsRoutes (UnitEditRoute unit.id))) ] [ text "Edit" ]
        , span [] [ text " | " ]
        , a [ href "javascript:void(0);", onClick (NavigateRoute (UnitsRoutes UnitIndexRoute)) ] [ text "Units list" ]
        ]


unitForm : Model -> Maybe String -> Html Msg
unitForm model maybeKey =
    let
        headerMessage =
            if maybeKey == Nothing then
                "New unit"
            else
                "Editing unit with id: " ++ (Maybe.withDefault "" maybeKey)
    in
        Html.form [ class "ui form", onSubmit (SubmitUnitForm (Maybe.withDefault "" maybeKey)) ]
            [ div [ class "ui stacked segment" ]
                [ h1 [] [ text headerMessage ]
                , div
                    [ classList
                        [ ( "field", True ), ( "error", model.unitFormErrors.name /= Nothing ) ]
                    ]
                    [ label [] [ text "Name" ]
                    , input
                        [ type_ "text"
                        , value model.unitFormFields.name
                        , onInput NameInputChanged
                        ]
                        []
                    , span [] [ text <| Maybe.withDefault "" model.unitFormErrors.name ]
                    ]
                , div
                    [ classList
                        [ ( "field", True ), ( "error", model.unitFormErrors.initials /= Nothing ) ]
                    ]
                    [ label [] [ text "Inventory" ]
                    , input
                        [ type_ "text"
                        , value model.unitFormFields.initials
                        , onInput InitialsInputChanged
                        ]
                        []
                    , span [] [ text <| Maybe.withDefault "" model.unitFormErrors.initials ]
                    ]
                , div []
                    [ label [] []
                    , button [ type_ "submit", class "ui  submit button" ] [ text "Save" ]
                    , a [ class "ui button", onClick RedirectBack ] [ text "Cancel" ]
                    ]
                ]
            , a [ href "javascript:void(0);", onClick (NavigateRoute (UnitsRoutes UnitIndexRoute)) ] [ text "Units list" ]
            ]


unitFormErrorPanel : Model -> Html a
unitFormErrorPanel model =
    let
        list : List FieldError
        list =
            [ { fieldName = "Name", errorMessage = model.unitFormErrors.name }
            , { fieldName = "Initials", errorMessage = model.unitFormErrors.initials }
            ]

        elementsWithError : List FieldError
        elementsWithError =
            List.filter (\el -> el.errorMessage /= Nothing) list
    in
        if model.unitFormShowErrorPanel then
            div [ id "formErrors", class "ui message error " ]
                [ div [ class "header" ] [ text "We had some issues:" ]
                , ul [ class "list" ] (List.map (\el -> li [] [ text (el.fieldName ++ ":" ++ (Maybe.withDefault "" el.errorMessage)) ]) elementsWithError)
                ]
        else
            div [] []
