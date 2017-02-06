port module Material exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Navigation
import Unit


-- model
--form model


type alias Model =
    { id : String
    , name : String
    , nameError : Maybe String
    , unit : Maybe Unit.Unit
    , unitError : Maybe String
    , inventory : String
    , inventoryError : Maybe String
    , material : Material
    , materials : List Material
    , query : String
    , error : Maybe String
    }


type Subpage
    = IndexPage
    | NewPage
    | EditPage String
    | ShowPage String



-- real model


type alias Material =
    { id : String
    , name : String
    , unit : Maybe Unit.Unit
    , inventory : Float
    }


initMaterial : Material
initMaterial =
    { id = ""
    , name = ""
    , unit = Nothing
    , inventory = 0.0
    }


initModel : Model
initModel =
    { id = ""
    , name = ""
    , nameError = Nothing
    , unit = Nothing
    , unitError = Nothing
    , inventory = ""
    , inventoryError = Nothing
    , material = initMaterial
    , materials = []
    , query = ""
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = NameInputChanged String
    | UnitSelectChanged Unit.Unit
    | InventoryInputChanged String
    | Add Material
    | Added Material
    | Update Material
    | Updated Material
    | Remove Material
    | Removed String


updateMaterialIfIdMatches : Material -> String -> Material -> Material
updateMaterialIfIdMatches updatedMaterial materialId originalMaterial =
    if originalMaterial.id == materialId then
        updatedMaterial
    else
        originalMaterial


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
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
                , Nothing
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
                , Nothing
                )

        UnitSelectChanged unit_ ->
            let
                actualModel =
                    model.material

                updatedModel =
                    { actualModel | unit = Just unit_ }
            in
                ( { model
                    | unit = Just unit_
                    , unitError = Nothing
                    , material = updatedModel
                  }
                , Cmd.none
                , Nothing
                )

        Add material ->
            ( model, addMaterial material, Nothing )

        Added material ->
            let
                newMaterials =
                    material :: model.materials
            in
                ( { model | materials = newMaterials }, Navigation.newUrl "#/materials", Just "Success added" )

        Remove material ->
            ( model, removeMaterial material, Nothing )

        Removed id ->
            let
                newMaterials =
                    List.filter (\material -> material.id /= id)
                        model.materials
            in
                ( { model | materials = newMaterials }, Cmd.none, Just "Success Removed" )

        Update material ->
            ( model, updateMaterial material, Nothing )

        Updated material ->
            -- create update material
            let
                -- map puts the element as last parameter for function updateMaterialIfIdMatches
                newMaterials =
                    List.map (updateMaterialIfIdMatches material material.id) model.materials

                -- this function updates a material element IF the id matches
            in
                ( { model | materials = newMaterials }, Navigation.newUrl "#/materials", Just "Success updated" )


view : Model -> Subpage -> Html Msg
view model subpage =
    div [ class "main" ]
        [ h1 [ class "ui header" ] [ text "Add runner" ]
        , errorPanel model.error
        , renderSubPage model subpage
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
    materials
        |> List.map materialView
        |> tbody []
        |> (\r -> materialsHeader :: [ r ])
        |> table []


materialsHeader : Html Msg
materialsHeader =
    thead []
        [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Name" ]
            , th [] [ text "Inventory" ]
            , th [] [ text "Actions" ]
            ]
        ]


materialView : Material -> Html Msg
materialView { id, name, inventory } =
    tr []
        [ td [] [ text id ]
        , td [] [ text name ]
        , td [] [ text <| toString inventory ]
        , td [] [ text "actions" ]
        ]


showView : Model -> String -> Html Msg
showView model key =
    div [] [ text ("Showing material id:" ++ key) ]


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
                    , button [ type_ "submit", class "ui fluid large teal submit button" ] [ text "Login" ]
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
