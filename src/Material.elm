port module Material exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Navigation
import Unit
import Message


-- model
--form model


type alias Model =
    { id : String
    , name : String
    , nameError : Maybe String
    , unit_id : String
    , unitError : Maybe String
    , inventory : String
    , inventoryError : Maybe String
    , material : Material
    , materials : List Material
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


type alias Material =
    { id : String
    , name : String
    , unit_id : Maybe String
    , inventory : Float
    }


initMaterial : Material
initMaterial =
    { id = ""
    , name = ""
    , unit_id = Nothing
    , inventory = 0.0
    }


initModel : Model
initModel =
    { id = ""
    , name = ""
    , nameError = Nothing
    , unit_id = ""
    , unitError = Nothing
    , inventory = ""
    , inventoryError = Nothing
    , material = initMaterial
    , materials = []
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
    | UnitSelectChanged String
    | InventoryInputChanged String
    | Add Material
    | Added Material
    | Update Material
    | Updated Material
    | Remove Material
    | Removed String


updateMaterialAtId : Material -> String -> Material -> Material
updateMaterialAtId updatedMaterial materialId originalMaterial =
    if originalMaterial.id == materialId then
        --need to finish: unit = Get unit from list passing unit id
        { originalMaterial | name = updatedMaterial.name, inventory = updatedMaterial.inventory }
    else
        originalMaterial


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
                        material =
                            getMaterialByIdFromMaterialsList model.materials id

                        materialOrInitMaterial =
                            Maybe.withDefault initMaterial material
                    in
                        --need to finish: unit = Get unit from list passing unit id
                        ( { model | id = id, unit_id = Maybe.withDefault "" materialOrInitMaterial.unit_id, name = materialOrInitMaterial.name, inventory = toString (materialOrInitMaterial.inventory), material = materialOrInitMaterial }
                        , Navigation.newUrl <| subpageToHash subpage
                        , Message.initModel
                        )

                _ ->
                    ( { model | material = initMaterial, name = "", inventory = "", unit_id = "" }
                    , Navigation.newUrl <| subpageToHash subpage
                    , Message.initModel
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
                , Message.initModel
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
                , Message.initModel
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
                , Message.initModel
                )

        Add material ->
            ( model, addMaterial material, Message.initModel )

        Added material ->
            let
                newMaterials =
                    material :: model.materials

                initMessage =
                    Message.initModel

                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess added"
                        , text = "Material successfully added"
                        , active = True
                    }
            in
                ( { model | materials = newMaterials }, Navigation.newUrl "#/materials", message )

        Remove material ->
            ( model, removeMaterial material, Message.initModel )

        Removed id ->
            let
                newMaterials =
                    List.filter (\material -> material.id /= id)
                        model.materials

                initMessage =
                    Message.initModel

                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess removed"
                        , text = "Material successfully removed"
                        , active = True
                    }
            in
                ( { model | materials = newMaterials }, Cmd.none, message )

        Update material ->
            ( model, updateMaterial material, Message.initModel )

        Updated material ->
            -- create update material
            let
                -- map puts the element as last parameter for function updateMaterialIfIdMatches
                newMaterials =
                    List.map (updateMaterialAtId material material.id) model.materials

                -- this function updates a material element IF the id matches
                initMessage =
                    Message.initModel

                message =
                    { initMessage
                        | messageClass = "positive"
                        , header = "Sucess updated"
                        , text = "Material successfully updated"
                        , active = True
                    }
            in
                ( { model | materials = newMaterials }, Navigation.newUrl "#/materials", message )


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
--     = MaterialIndexPage
--     | MaterialNewPage
--     | MaterialEditPage Int
--     | MaterialShowPage Int


listView : Model -> Html Msg
listView { query, materials } =
    -- filter using query afterwards
    let
        table_ =
            materials
                |> List.map materialView
                |> tbody []
                |> (\r -> materialsHeader :: [ r ])
                |> table [ class "ui celled table" ]
    in
        div []
            [ h1 [ class "ui header" ] [ text "Materials list" ]
            , table_
            , button [ class "ui button", onClick (Navigate NewPage Message.initModel) ] [ text "New" ]
            ]


materialsHeader : Html Msg
materialsHeader =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Unit ID" ]
            , th [] [ text "Inventory" ]
            , th [] [ text "Actions" ]
            ]
        ]


materialView : Material -> Html Msg
materialView material =
    tr []
        [ td [] [ text material.id ]
        , td [] [ text material.name ]
        , td [] [ text (Maybe.withDefault "" material.unit_id) ]
        , td [] [ text <| toString material.inventory ]
        , td []
            [ button [ class "ui button", onClick (Navigate (EditPage material.id) Message.initModel) ] [ text "Edit" ]
            , button [ class "ui button", onClick (Navigate (ShowPage material.id) Message.initModel) ] [ text "Show" ]
            , button [ class "ui button", onClick (Remove material) ] [ text "Remove" ]
            ]
        ]


showView : Model -> String -> Html Msg
showView model key =
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


formView : Model -> Maybe String -> Html Msg
formView model maybeKey =
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
                    , input
                        [ type_ "text"
                        , value model.unit_id
                        , onInput UnitSelectChanged
                        ]
                        []
                    , span [] [ text <| Maybe.withDefault "" model.inventoryError ]
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
