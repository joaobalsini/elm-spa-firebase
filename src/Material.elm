port module Material exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Aliases exposing (Unit, Material, Message, initMaterial, initMessage)
import Navigation


-- model
--form model


type alias Model =
    { name : String
    , nameError : Maybe String
    , unit_id : String
    , unitError : Maybe String
    , inventory : String
    , inventoryError : Maybe String
    , material : Material
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
            "#/materials"

        NewPage ->
            "#/materials/new"

        ShowPage id ->
            "#/materials/" ++ id

        EditPage id ->
            "#/materials/edit/" ++ id



-- real model


initModel : Model
initModel =
    { name = ""
    , nameError = Nothing
    , unit_id = ""
    , unitError = Nothing
    , inventory = ""
    , inventoryError = Nothing
    , material = initMaterial
    , query = ""
    , error = Nothing
    , displayingConfirmDialog = False
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = Navigate Subpage (List Material) Message
    | NameInputChanged String
    | UnitSelectChanged String
    | InventoryInputChanged String
    | Add Material
    | Added Material
    | Update Material
    | Updated Material
    | Remove Material
    | Removed String


getMaterialByIdFromMaterialsList : List Material -> String -> Maybe Material
getMaterialByIdFromMaterialsList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getMaterialByIdFromMaterialsList xs id


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        Navigate subpage materials message ->
            case subpage of
                ShowPage id ->
                    ( model
                    , Navigation.newUrl <| subpageToHash subpage
                    , initMessage
                    )

                EditPage id ->
                    let
                        material =
                            getMaterialByIdFromMaterialsList materials id

                        materialOrInitMaterial =
                            Maybe.withDefault initMaterial material
                    in
                        --need to finish: unit = Get unit from list passing unit id
                        ( { model | unit_id = Maybe.withDefault "" materialOrInitMaterial.unit_id, name = materialOrInitMaterial.name, inventory = toString (materialOrInitMaterial.inventory), material = materialOrInitMaterial }
                        , Navigation.newUrl <| subpageToHash subpage
                        , initMessage
                        )

                _ ->
                    ( { model | material = initMaterial, name = "", inventory = "", unit_id = "" }
                    , Navigation.newUrl <| subpageToHash subpage
                    , initMessage
                    )

        NameInputChanged name_ ->
            let
                actualModel =
                    model.material

                updatedModel =
                    { actualModel | name = name_ }
            in
                ( { model
                    | name = name_
                    , nameError = Nothing
                    , material = updatedModel
                  }
                , Cmd.none
                , initMessage
                )

        InventoryInputChanged inventory_ ->
            let
                actualModel =
                    model.material

                inventoryFloat =
                    inventory_
                        |> String.toFloat
                        |> Result.withDefault 0.0

                inventoryError =
                    if inventoryFloat <= 0 then
                        Just "Must Enter a Positive Number"
                    else
                        Nothing

                updatedModel =
                    { actualModel | inventory = inventoryFloat }
            in
                ( { model
                    | inventory = inventory_
                    , inventoryError = inventoryError
                    , material = updatedModel
                  }
                , Cmd.none
                , initMessage
                )

        UnitSelectChanged unit_id_ ->
            let
                actualModel =
                    model.material

                updatedModel =
                    { actualModel | unit_id = Just unit_id_ }
            in
                ( { model
                    | unit_id = unit_id_
                    , unitError = Nothing
                    , material = updatedModel
                  }
                , Cmd.none
                , initMessage
                )

        Add material ->
            ( model, addMaterial material, initMessage )

        Added material ->
            let
                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess added"
                        , text = "Material successfully added"
                        , active = True
                    }
            in
                ( { model | material = initMaterial }, Navigation.newUrl "#/materials", message )

        Remove material ->
            ( model, removeMaterial material, initMessage )

        Removed id ->
            let
                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess removed"
                        , text = "Material successfully removed"
                        , active = True
                    }
            in
                ( model, Cmd.none, message )

        Update material ->
            ( model, updateMaterial material, initMessage )

        Updated material ->
            -- create update material
            let
                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess updated"
                        , text = "Material successfully updated"
                        , active = True
                    }
            in
                ( { model | material = initMaterial }, Navigation.newUrl "#/materials", message )


view : Model -> List Material -> List Unit -> Subpage -> Html Msg
view model materials units subpage =
    let
        page =
            case subpage of
                IndexPage ->
                    materialsToTable materials model

                NewPage ->
                    materialForm model units Nothing

                EditPage string ->
                    materialForm model units (Just string)

                ShowPage string ->
                    materialShow model string
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


unitOption : Unit -> Html Msg
unitOption unit =
    option [ value unit.id ] [ text (unit.name ++ "(" ++ unit.initials ++ ")") ]


materialsToTable : List Material -> Model -> Html Msg
materialsToTable materials model =
    -- filter using query afterwards
    let
        table_ =
            List.map (materialToTr materials) materials
                |> tbody []
                |> (\r -> materialsTh :: [ r ])
                |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Materials list" ]
            , table_
            , button [ class "ui button", onClick (Navigate NewPage materials initMessage) ] [ text "New" ]
            ]


materialsTh : Html Msg
materialsTh =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Unit ID" ]
            , th [] [ text "Inventory" ]
            , th [] [ text "Actions" ]
            ]
        ]


materialToTr : List Material -> Material -> Html Msg
materialToTr materials material =
    tr []
        [ td [] [ text material.id ]
        , td [] [ text material.name ]
        , td [] [ text (Maybe.withDefault "" material.unit_id) ]
        , td [] [ text <| toString material.inventory ]
        , td []
            [ button [ class "ui button", onClick (Navigate (EditPage material.id) materials initMessage) ] [ text "Edit" ]
            , button [ class "ui button", onClick (Navigate (ShowPage material.id) materials initMessage) ] [ text "Show" ]
            , button [ class "ui button", onClick (Remove material) ] [ text "Remove" ]
            ]
        ]


materialShow : Model -> String -> Html Msg
materialShow model key =
    div [] [ text ("Showing material id:" ++ key) ]


confirmModalView : Model -> Html Msg
confirmModalView model =
    div [ class "ui modal hidden" ]
        [ div [ class "ui header" ] [ text "Are you sure?" ]
        , div [ class "actions" ]
            [ div [ class "ui red cancel button" ] [ text "Nope" ]
            , div [ class "ui green ok button" ] [ text "Yep" ]
            ]
        ]


materialForm : Model -> List Unit -> Maybe String -> Html Msg
materialForm model units maybeKey =
    let
        ( action, headerMessage ) =
            if maybeKey == Nothing then
                ( Add model.material, "New material" )
            else
                ( Update model.material, "Editing material with id: " ++ (Maybe.withDefault "" maybeKey) )
    in
        Html.form [ class "ui large form", onSubmit action ]
            [ div [ class "ui stacked segment" ]
                [ h1 [] [ text headerMessage ]
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
                    [ label [] [ text "UnitId" ]
                    , select [ onInput UnitSelectChanged ]
                        (List.map unitOption units)
                    , span [] [ text <| Maybe.withDefault "" model.unitError ]
                    ]
                , div [ class "field" ]
                    [ label [] [ text "Inventory" ]
                    , input
                        [ type_ "text"
                        , value model.inventory
                        , onInput InventoryInputChanged
                        ]
                        []
                    , span [] [ text <| Maybe.withDefault "" model.inventoryError ]
                    ]
                , div []
                    [ label [] []
                    , button [ type_ "submit", class "ui fluid large teal submit button" ] [ text "Save" ]
                    ]
                ]
            ]


port addMaterial : Material -> Cmd msg


port materialAdded : (Material -> msg) -> Sub msg


port updateMaterial : Material -> Cmd msg


port materialUpdated : (Material -> msg) -> Sub msg


port removeMaterial : Material -> Cmd msg


port materialRemoved : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ materialAdded Added
        , materialUpdated Updated
        , materialRemoved Removed
        ]
