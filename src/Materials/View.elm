module Materials.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Routes exposing (..)
import Materials.Routes exposing (..)
import Materials.Model exposing (..)
import Materials.Msgs exposing (..)
import Units.Model exposing (Unit, initUnit, getUnitByIdFromList)
import Utils.CommonDefinitions exposing (FieldError)
import Utils.StringNumberConversion exposing (floatToStringBrazilFormat)
import Units.Routes


view : Model -> Maybe (List Material) -> Maybe (List Unit) -> MaterialRoute -> Html Msg
view model maybeMaterials maybeUnits subroute =
    case maybeMaterials of
        Nothing ->
            div [ class "ui active inverted dimmer" ]
                [ div [ class "ui text loader" ] [ text "Loading materials" ]
                ]

        Just materials ->
            case maybeUnits of
                Nothing ->
                    div [ class "ui active inverted dimmer" ]
                        [ div [ class "ui text loader" ] [ text "Loading units" ] ]

                Just units ->
                    let
                        page =
                            case subroute of
                                MaterialIndexRoute ->
                                    materialsToTable materials units model

                                MaterialNewRoute ->
                                    materialForm model units Nothing

                                MaterialEditRoute id ->
                                    let
                                        maybeMaterial =
                                            Materials.Model.getMaterialByIdFromList materials id
                                    in
                                        case maybeMaterial of
                                            Nothing ->
                                                text ("Material not found with id:" ++ id)

                                            Just material ->
                                                materialForm model units (Just material)

                                MaterialShowRoute id ->
                                    let
                                        maybeMaterial =
                                            Materials.Model.getMaterialByIdFromList materials id
                                    in
                                        case maybeMaterial of
                                            Nothing ->
                                                text ("Material not found with id:" ++ id)

                                            Just material ->
                                                materialShow material units
                    in
                        div [ class "main" ]
                            [ page
                            ]


unitOption : String -> Unit -> Html Msg
unitOption selectedUnitId unit =
    if unit.name == "" then
        option [ value "-1", selected (selectedUnitId == "") ] [ text "Please select one unit" ]
    else
        option [ value unit.id, selected (selectedUnitId == unit.id) ] [ text (unit.name ++ "(" ++ unit.initials ++ ")") ]


materialsToTable : List Material -> List Unit -> Model -> Html Msg
materialsToTable materials units model =
    -- filter using query afterwards
    let
        table_ =
            List.map (materialToTr units) materials
                |> tbody []
                |> (\r -> materialsTh :: [ r ])
                |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Materials list" ]
            , table_
            , button [ class "ui button", onClick (NavigateRoute (MaterialsRoutes MaterialNewRoute)) ] [ text "New" ]
            ]


materialsTh : Html Msg
materialsTh =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Unit" ]
            , th [] [ text "Inventory" ]
            , th [] [ text "Actions" ]
            ]
        ]


materialToTr : List Unit -> Material -> Html Msg
materialToTr units material =
    let
        unit_name =
            case getUnitByIdFromList units (Maybe.withDefault "" material.unit_id) of
                Nothing ->
                    ""

                Just unit ->
                    unit.name
    in
        tr []
            [ td [] [ text material.id ]
            , td [] [ text material.name ]
            , td [] [ text unit_name ]
            , td [] [ text <| floatToStringBrazilFormat material.inventory ]
            , td []
                [ button [ class "ui button", onClick (NavigateRoute (MaterialsRoutes (MaterialEditRoute material.id))) ] [ text "Edit" ]
                , button [ class "ui button", onClick (NavigateRoute (MaterialsRoutes (MaterialShowRoute material.id))) ] [ text "Show" ]
                , button [ class "ui button", onClick (Remove material.id) ] [ text "Remove" ]
                ]
            ]


materialShow : Material -> List Unit -> Html Msg
materialShow material units =
    let
        ( unit_id, unit_name, unit_initials ) =
            case getUnitByIdFromList units (Maybe.withDefault "" material.unit_id) of
                Nothing ->
                    ( "", "Unit not found", "" )

                Just unit ->
                    ( unit.id, unit.name, unit.initials )
    in
        div []
            [ div [ class "ui stacked segment" ]
                [ h1 [] [ text ("Showing material id:" ++ material.id) ]
                , h4 [] [ text "Name" ]
                , p [] [ text material.name ]
                , h4 [] [ text "Represented in unit" ]
                , p []
                    [ span [] [ text unit_name ]
                    , span [] [ text " " ]
                    , span [] [ a [ href "javascript:void(0);", onClick (NavigateRoute (UnitsRoutes (Units.Routes.UnitShowRoute unit_id))) ] [ text "View" ] ]
                    ]
                , h4 [] [ text "Inventory" ]
                , p [] [ text ((floatToStringBrazilFormat material.inventory) ++ unit_initials) ]
                ]
            , a [ href "javascript:void(0);", onClick (RedirectBack) ] [ text "Back" ]
            , span [] [ text " | " ]
            , a [ href "javascript:void(0);", onClick (NavigateRoute (MaterialsRoutes (MaterialEditRoute material.id))) ] [ text "Edit" ]
            , span [] [ text " | " ]
            , a [ href "javascript:void(0);", onClick (NavigateRoute (MaterialsRoutes MaterialIndexRoute)) ] [ text "Materials list" ]
            ]


materialForm : Model -> List Unit -> Maybe Material -> Html Msg
materialForm model units maybeMaterial =
    let
        ( headerNotification, material, submitMsg ) =
            case maybeMaterial of
                Nothing ->
                    ( "New material", Nothing, SubmitMaterialForm Nothing )

                Just material ->
                    ( "Editing material with id: " ++ (material.id), Just material, SubmitMaterialForm (Just material) )
    in
        div []
            [ materialFormErrorPanel model
            , Html.form [ class "ui form", onSubmit submitMsg ]
                [ div [ class "ui stacked segment" ]
                    [ h1 [] [ text headerNotification ]
                    , div
                        [ classList
                            [ ( "field", True ), ( "error", model.materialFormErrors.name /= Nothing ) ]
                        ]
                        [ label [] [ text "Name" ]
                        , input
                            [ type_ "text"
                            , value model.materialFormFields.name
                            , onInput NameInputChanged
                            ]
                            []
                        , span [] [ text <| Maybe.withDefault "" model.materialFormErrors.name ]
                        ]
                    , div
                        [ classList
                            [ ( "field", True ), ( "error", model.materialFormErrors.unit_id /= Nothing ) ]
                        ]
                        [ label [] [ text "UnitId" ]
                        , select [ onInput UnitSelectChanged ]
                            (List.map (unitOption model.materialFormFields.unit_id) (units ++ [ initUnit ]))
                        , span [] [ text <| Maybe.withDefault "" model.materialFormErrors.unit_id ]
                        ]
                    , div
                        [ classList
                            [ ( "field", True ), ( "error", model.materialFormErrors.inventory /= Nothing ) ]
                        ]
                        [ label [] [ text "Inventory" ]
                        , input
                            [ type_ "text"
                            , value model.materialFormFields.inventory
                            , onInput InventoryInputChanged
                            ]
                            []
                        , span [] [ text <| Maybe.withDefault "" model.materialFormErrors.inventory ]
                        ]
                    , div []
                        [ label [] []
                        , button [ type_ "submit", class "ui submit button" ] [ text "Save" ]
                        , a [ class "ui button", onClick RedirectBack ] [ text "Cancel" ]
                        ]
                    ]
                , a [ href "javascript:void(0);", onClick (NavigateRoute (MaterialsRoutes MaterialIndexRoute)) ] [ text "Materials list" ]
                ]
            ]


materialFormErrorPanel : Model -> Html a
materialFormErrorPanel model =
    let
        list : List FieldError
        list =
            [ { fieldName = "Name", errorNotification = model.materialFormErrors.name }
            , { fieldName = "UnitId", errorNotification = model.materialFormErrors.unit_id }
            , { fieldName = "Inventory", errorNotification = model.materialFormErrors.inventory }
            ]

        elementsWithError : List FieldError
        elementsWithError =
            List.filter (\el -> el.errorNotification /= Nothing) list
    in
        if model.materialFormShowErrorPanel then
            div [ id "formErrors", class "ui notification error " ]
                [ div [ class "header" ] [ text "We had some issues:" ]
                , ul [ class "list" ] (List.map (\el -> li [] [ text (el.fieldName ++ ":" ++ (Maybe.withDefault "" el.errorNotification)) ]) elementsWithError)
                ]
        else
            div [] []
