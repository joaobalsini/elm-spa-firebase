port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), top)
import Material
import Unit
import Index
import Login
import Message
import Aliases


main : Program Never Model Msg
main =
    Navigation.program locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- model


type alias Model =
    { route : Route
    , login : Login.Model
    , index : Index.Model
    , unit : Unit.Model
    , message : Message.Model
    , material : Material.Model
    , units : List Aliases.Unit
    , materials : List Aliases.Material
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            locationToRoute location

        ( indexInitModel, indexCmd ) =
            Index.init

        ( loginInitModel, loginCmd ) =
            Login.init

        ( unitInitModel, unitCmd ) =
            Unit.init

        ( materialInitModel, materialCmd ) =
            Material.init

        ( messageInitModel, messageCmd ) =
            Message.init

        initModel =
            { route = route
            , login = loginInitModel
            , index = indexInitModel
            , message = Message.initModel
            , unit = unitInitModel
            , units = []
            , material = materialInitModel
            , materials = []
            }

        cmds =
            Cmd.batch
                [ Cmd.map LoginMsg loginCmd
                , Cmd.map IndexMsg indexCmd
                , Cmd.map UnitMsg unitCmd
                , Cmd.map MaterialMsg materialCmd
                , Cmd.map MessageMsg messageCmd
                ]
    in
        ( initModel, cmds )



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
        , Url.map LoginRoute (Url.s "login")
        , Url.map UnitEditRoute (Url.s "units" </> Url.s "edit" </> Url.string)
        , Url.map UnitNewRoute (Url.s "units" </> Url.s "new")
        , Url.map UnitShowRoute (Url.s "units" </> Url.string)
        , Url.map UnitIndexRoute (Url.s "units")
        , Url.map MaterialEditRoute (Url.s "materials" </> Url.s "edit" </> Url.string)
        , Url.map MaterialNewRoute (Url.s "materials" </> Url.s "new")
        , Url.map MaterialShowRoute (Url.s "materials" </> Url.string)
        , Url.map MaterialIndexRoute (Url.s "materials")
        ]


locationToRoute : Location -> Route
locationToRoute location =
    case (Url.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



-- type Route
--     = IndexRoute
--     | LoginRoute
--     | UnitIndexRoute
--     | UnitShowRoute String
--     | UnitNewRoute
--     | UnitEditRoute String
--     | MaterialIndexRoute
--     | MaterialShowRoute String
--     | MaterialNewRoute
--     | MaterialEditRoute String
--     | NotFoundRoute


routeToHash : Route -> String
routeToHash route =
    case route of
        IndexRoute ->
            "#/"

        LoginRoute ->
            "#/login"

        UnitIndexRoute ->
            "#/units"

        UnitNewRoute ->
            "#/units/new"

        UnitShowRoute id ->
            "#/units/" ++ id

        UnitEditRoute id ->
            "#/units/edit/" ++ id

        MaterialIndexRoute ->
            "#/materials"

        MaterialNewRoute ->
            "#/materials/new"

        MaterialShowRoute id ->
            "#/materials/" ++ id

        MaterialEditRoute id ->
            "#/materials/edit/" ++ id

        NotFoundRoute ->
            "#notfound"


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location
        |> locationToRoute
        |> ChangePage location.hash


updateUnitAtId : Aliases.Unit -> String -> Aliases.Unit -> Aliases.Unit
updateUnitAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


getUnitByIdFromList : List Aliases.Unit -> String -> Maybe Aliases.Unit
getUnitByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getUnitByIdFromList xs id


updateMaterialAtId : Aliases.Material -> String -> Aliases.Material -> Aliases.Material
updateMaterialAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


getMaterialByIdFromList : List Aliases.Material -> String -> Maybe Aliases.Material
getMaterialByIdFromList list id =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x.id == id then
                Just x
            else
                getMaterialByIdFromList xs id



-- update


type Msg
    = Navigate Route
    | ChangePage String Route
    | IndexMsg Index.Msg
    | LoginMsg Login.Msg
    | UnitMsg Unit.Msg
    | MaterialMsg Material.Msg
    | MessageMsg Message.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate route ->
            ( { model | route = route }, Navigation.newUrl <| routeToHash route )

        ChangePage locationHashIfNotFound route ->
            (if route == NotFoundRoute && locationHashIfNotFound /= "#notfound" then
                ( { model | route = route }, Navigation.newUrl "#notfound" )
             else
                ( { model | route = route }, Cmd.none )
            )

        IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    Index.update msg model.index
            in
                ( { model | index = indexModel }
                , Cmd.map IndexMsg cmd
                )

        LoginMsg msg ->
            let
                ( loginModel, cmd ) =
                    Login.update msg model.login
            in
                ( { model | login = loginModel }
                , Cmd.map LoginMsg cmd
                )

        UnitMsg msg ->
            case msg of
                Unit.Added unit ->
                    let
                        newUnits =
                            unit :: model.units

                        ( unitModel, cmd, message ) =
                            Unit.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message, units = newUnits }
                        , Cmd.map UnitMsg cmd
                        )

                Unit.Updated updatedUnit ->
                    let
                        newUnits =
                            List.map (updateUnitAtId updatedUnit updatedUnit.id) model.units

                        ( unitModel, cmd, message ) =
                            Unit.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message, units = newUnits }
                        , Cmd.map UnitMsg cmd
                        )

                Unit.Removed id ->
                    let
                        newUnits =
                            List.filter (\unit -> unit.id /= id)
                                model.units

                        ( unitModel, cmd, message ) =
                            Unit.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message, units = newUnits }
                        , Cmd.map UnitMsg cmd
                        )

                _ ->
                    let
                        ( unitModel, cmd, message ) =
                            Unit.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message }
                        , Cmd.map UnitMsg cmd
                        )

        MaterialMsg msg ->
            case msg of
                Material.Added material ->
                    let
                        newMaterials =
                            material :: model.materials

                        ( materialModel, cmd, message ) =
                            Material.update msg model.material
                    in
                        ( { model | material = materialModel, message = message, materials = newMaterials }
                        , Cmd.map MaterialMsg cmd
                        )

                Material.Updated updatedMaterial ->
                    let
                        newMaterials =
                            List.map (updateMaterialAtId updatedMaterial updatedMaterial.id) model.materials

                        ( materialModel, cmd, message ) =
                            Material.update msg model.material
                    in
                        ( { model | material = materialModel, message = message, materials = newMaterials }
                        , Cmd.map MaterialMsg cmd
                        )

                Material.Removed id ->
                    let
                        newMaterials =
                            List.filter (\material -> material.id /= id)
                                model.materials

                        ( materialModel, cmd, message ) =
                            Material.update msg model.material
                    in
                        ( { model | material = materialModel, message = message, materials = newMaterials }
                        , Cmd.map MaterialMsg cmd
                        )

                _ ->
                    let
                        ( materialModel, cmd, message ) =
                            Material.update msg model.material
                    in
                        ( { model | material = materialModel, message = message }
                        , Cmd.map MaterialMsg cmd
                        )

        MessageMsg msg ->
            let
                ( messageModel, cmd ) =
                    Message.update msg model.message
            in
                ( { model | message = messageModel }
                , Cmd.map MessageMsg cmd
                )



-- view
-- type Route
--     = IndexRoute
--     | LoginRoute
--     | UnitIndexRoute
--     | UnitShowRoute String
--     | UnitNewRoute
--     | UnitEditRoute String
--     | MaterialIndexRoute
--     | MaterialShowRoute String
--     | MaterialNewRoute
--     | MaterialEditRoute String
--     | NotFoundRoute


view : Model -> Html Msg
view model =
    let
        page =
            case model.route of
                IndexRoute ->
                    Html.map IndexMsg
                        (Index.view model.index)

                LoginRoute ->
                    Html.map LoginMsg
                        (Login.view model.login)

                UnitIndexRoute ->
                    Html.map UnitMsg
                        (Unit.view model.unit model.units Unit.IndexPage)

                UnitShowRoute id ->
                    Html.map UnitMsg
                        (Unit.view model.unit model.units (Unit.ShowPage id))

                UnitNewRoute ->
                    Html.map UnitMsg
                        (Unit.view model.unit model.units Unit.NewPage)

                UnitEditRoute id ->
                    Html.map UnitMsg
                        (Unit.view model.unit model.units (Unit.EditPage id))

                MaterialIndexRoute ->
                    Html.map MaterialMsg
                        (Material.view model.material model.materials model.units Material.IndexPage)

                MaterialShowRoute id ->
                    Html.map MaterialMsg
                        (Material.view model.material model.materials model.units (Material.ShowPage id))

                MaterialNewRoute ->
                    Html.map MaterialMsg
                        (Material.view model.material model.materials model.units Material.NewPage)

                MaterialEditRoute id ->
                    Html.map MaterialMsg
                        (Material.view model.material model.materials model.units (Material.EditPage id))

                NotFoundRoute ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
        div []
            [ div [ class "ui fixed inverted menu" ] [ pageHeader model ]
            , Html.map MessageMsg (Message.view model.message)
            , div [ class "ui main text container" ] [ page ]
            ]


pageHeader : Model -> Html Msg
pageHeader model =
    div [ class "ui container" ]
        [ a [ class "item", onClick (Navigate IndexRoute) ] [ text "Index" ]
        , a [ class "item", onClick (Navigate MaterialIndexRoute) ] [ text "Materials" ]
        , a [ class "item", onClick (Navigate UnitIndexRoute) ] [ text "Units" ]
        , a [ class "item right", onClick (Navigate LoginRoute) ] [ text "Login" ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        loginSub =
            Login.subscriptions model.login

        indexSub =
            Index.subscriptions model.index

        unitSub =
            Unit.subscriptions model.unit

        materialSub =
            Material.subscriptions model.material

        messageSub =
            Message.subscriptions model.message
    in
        Sub.batch
            [ Sub.map IndexMsg indexSub
            , Sub.map LoginMsg loginSub
            , Sub.map UnitMsg unitSub
            , Sub.map MaterialMsg materialSub
            , Sub.map MessageMsg messageSub
            ]
