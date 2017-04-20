port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), top)
import MaterialModule
import UnitModule
import Index
import Login
import Message
import Aliases exposing (..)
import Routes exposing (..)


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
    , lastRoute : Route
    , login : Login.Model
    , index : Index.Model
    , unit : UnitModule.Model
    , message : Message.Model
    , material : MaterialModule.Model
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
            UnitModule.init

        ( materialInitModel, materialCmd ) =
            MaterialModule.init

        ( messageInitModel, messageCmd ) =
            Message.init

        initModel =
            { route = route
            , lastRoute = IndexRoute
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
        Nothing ->
            NotFoundRoute

        Just route ->
            route



-- this function is triggered whenever the user changes the url


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location
        |> locationToRoute
        |> ChangePage


updateUnitAtId : Aliases.Unit -> String -> Aliases.Unit -> Aliases.Unit
updateUnitAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


updateMaterialAtId : Aliases.Material -> String -> Aliases.Material -> Aliases.Material
updateMaterialAtId updatedElement elementId originalElement =
    if originalElement.id == elementId then
        updatedElement
    else
        originalElement


parseMaterialFromDB : Model -> MaterialDB -> Material
parseMaterialFromDB model materialDb =
    let
        teste =
            Debug.log (Maybe.withDefault "" materialDb.unit_id) "parseMaterialFromDB"

        -- which fields needs special parsing first?
        material : Material
        material =
            { id = materialDb.id
            , name = materialDb.name
            , unit_id = materialDb.unit_id
            , inventory = materialDb.inventory
            }
    in
        material



-- update


type Msg
    = Navigate Route
    | ChangePage Route
    | IndexMsg Index.Msg
    | LoginMsg Login.Msg
    | UnitMsg UnitModule.Msg
    | MaterialMsg MaterialModule.Msg
    | MessageMsg Message.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Navigate is used once a user clicks in a link
        Navigate route ->
            let
                ( model_, msg_ ) =
                    update (ChangePage route) model
            in
                ( model_, Navigation.newUrl <| routeToHash route )

        -- ChangePage is used once a user changes the URL manually
        ChangePage route ->
            let
                lastRoute =
                    model.route
            in
                case route of
                    -- We need to process part of the message here, as the array of units is "Main module" responsability
                    -- Why don't we keep the array of units inside unitModel?
                    -- Because some other modules will also need it and we want to avoid duplications as it would tend to bug
                    -- We could also create "message interceptors" for each message of the subModule but I only need to pre process some of them
                    -- As the UnitShow and UnitEdit need to be pre validated (as the user could, for example)
                    -- And, in case of Unit Edit we NEED to set the unit inside the unit module as we need to interact of it inside the unit view, when inputsChanged for example
                    UnitShowRoute id ->
                        let
                            unit =
                                getUnitByIdFromList model.units id

                            -- if unit is not found redirect back and show error message
                            ( updatedModel, cmd ) =
                                if unit == Nothing then
                                    let
                                        ( updatedModel, cmd ) =
                                            update (Navigate lastRoute) model
                                    in
                                        ( { updatedModel | message = Aliases.errorMessage ("Unit with id " ++ id ++ " not found!") }, cmd )
                                else
                                    let
                                        ( unitModel, cmd, message ) =
                                            UnitModule.update (UnitModule.PrepareView (UnitModule.ShowPage (Maybe.withDefault Aliases.initUnit unit)) model.units Aliases.initMessage) model.unit
                                    in
                                        ( { model | route = route, lastRoute = lastRoute, unit = unitModel }, Cmd.none )
                        in
                            ( updatedModel, cmd )

                    UnitEditRoute id ->
                        let
                            unit =
                                getUnitByIdFromList model.units id

                            -- if unit is not found redirect back and show error message
                            ( updatedModel, cmd ) =
                                if unit == Nothing then
                                    let
                                        ( updatedModel, cmd ) =
                                            update (Navigate lastRoute) model
                                    in
                                        ( { updatedModel | message = Aliases.errorMessage ("Unit with id " ++ id ++ " not found!") }, cmd )
                                else
                                    let
                                        ( unitModel, cmd, message ) =
                                            UnitModule.update (UnitModule.PrepareView (UnitModule.EditPage (Maybe.withDefault Aliases.initUnit unit)) model.units Aliases.initMessage) model.unit
                                    in
                                        ( { model | route = route, lastRoute = lastRoute, unit = unitModel }, Cmd.none )
                        in
                            ( updatedModel, cmd )

                    MaterialShowRoute id ->
                        let
                            material =
                                getMaterialByIdFromList model.materials id

                            -- if material is not found redirect back and show error message
                            ( updatedModel, cmd ) =
                                if material == Nothing then
                                    let
                                        ( updatedModel, cmd ) =
                                            update (Navigate lastRoute) model
                                    in
                                        ( { updatedModel | message = Aliases.errorMessage ("Material with id " ++ id ++ " not found!") }, cmd )
                                else
                                    let
                                        ( materialModel, cmd, message ) =
                                            MaterialModule.update (MaterialModule.PrepareView (MaterialModule.ShowPage (Maybe.withDefault Aliases.initMaterial material)) model.materials Aliases.initMessage) model.material
                                    in
                                        ( { model | route = route, lastRoute = lastRoute, material = materialModel }, Cmd.none )
                        in
                            ( updatedModel, cmd )

                    MaterialEditRoute id ->
                        let
                            material =
                                getMaterialByIdFromList model.materials id

                            -- if material is not found redirect back and show error message
                            ( updatedModel, cmd ) =
                                if material == Nothing then
                                    let
                                        ( updatedModel, cmd ) =
                                            update (Navigate lastRoute) model
                                    in
                                        ( { updatedModel | message = Aliases.errorMessage ("Material with id " ++ id ++ " not found!") }, cmd )
                                else
                                    let
                                        ( materialModel, cmd, message ) =
                                            MaterialModule.update (MaterialModule.PrepareView (MaterialModule.EditPage (Maybe.withDefault Aliases.initMaterial material)) model.materials Aliases.initMessage) model.material
                                    in
                                        ( { model | route = route, lastRoute = lastRoute, material = materialModel }, Cmd.none )
                        in
                            ( updatedModel, cmd )

                    _ ->
                        ( { model | route = route, lastRoute = lastRoute }, Cmd.none )

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
            -- Here we handle the units list, below we do the same for the materials list --
            case msg of
                -- We "intercept" the message to unit, in case its Added, we add the unit to the units list and pass the message to unit module
                UnitModule.Added unit ->
                    let
                        newUnits =
                            unit :: model.units

                        ( unitModel, cmd, message ) =
                            UnitModule.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message, units = newUnits }
                        , Cmd.map UnitMsg cmd
                        )

                -- in case its Updated we update the unit in the units list and pass the message to unit module
                UnitModule.Updated updatedUnit ->
                    let
                        newUnits =
                            List.map (updateUnitAtId updatedUnit updatedUnit.id) model.units

                        ( unitModel, cmd, message ) =
                            UnitModule.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message, units = newUnits }
                        , Cmd.map UnitMsg cmd
                        )

                -- in case its Removed we remove the unit from the units list and pass the message to unit module
                UnitModule.Removed id ->
                    let
                        newUnits =
                            List.filter (\unit -> unit.id /= id)
                                model.units

                        ( unitModel, cmd, message ) =
                            UnitModule.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message, units = newUnits }
                        , Cmd.map UnitMsg cmd
                        )

                -- otherwise we just pass the message to unit module
                _ ->
                    let
                        ( unitModel, cmd, message ) =
                            UnitModule.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message }
                        , Cmd.map UnitMsg cmd
                        )

        MaterialMsg msg ->
            -- Here we handle the materials list, please check the units list above for comments
            case msg of
                MaterialModule.Added material ->
                    let
                        newMaterials =
                            parseMaterialFromDB model material :: model.materials

                        ( materialModel, cmd, message ) =
                            MaterialModule.update msg model.material
                    in
                        ( { model | material = materialModel, message = message, materials = newMaterials }
                        , Cmd.map MaterialMsg cmd
                        )

                MaterialModule.Updated updatedMaterial ->
                    let
                        newMaterials =
                            List.map (updateMaterialAtId (parseMaterialFromDB model updatedMaterial) updatedMaterial.id) model.materials

                        ( materialModel, cmd, message ) =
                            MaterialModule.update msg model.material
                    in
                        ( { model | material = materialModel, message = message, materials = newMaterials }
                        , Cmd.map MaterialMsg cmd
                        )

                MaterialModule.Removed id ->
                    let
                        newMaterials =
                            List.filter (\material -> material.id /= id)
                                model.materials

                        ( materialModel, cmd, message ) =
                            MaterialModule.update msg model.material
                    in
                        ( { model | material = materialModel, message = message, materials = newMaterials }
                        , Cmd.map MaterialMsg cmd
                        )

                _ ->
                    let
                        ( materialModel, cmd, message ) =
                            MaterialModule.update msg model.material
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
        -- get the page through the view method of each Module passing the parameters needed and render that page
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
                        (UnitModule.view model.unit model.units UnitModule.IndexPage)

                UnitShowRoute id ->
                    let
                        unitOrClearUnit =
                            Maybe.withDefault Aliases.initUnit (getUnitByIdFromList model.units id)
                    in
                        Html.map UnitMsg
                            (UnitModule.view model.unit model.units (UnitModule.ShowPage unitOrClearUnit))

                UnitNewRoute ->
                    Html.map UnitMsg
                        (UnitModule.view model.unit model.units (UnitModule.NewPage))

                UnitEditRoute id ->
                    let
                        unitOrClearUnit =
                            Maybe.withDefault Aliases.initUnit (getUnitByIdFromList model.units id)
                    in
                        Html.map UnitMsg
                            (UnitModule.view model.unit model.units (UnitModule.EditPage unitOrClearUnit))

                MaterialIndexRoute ->
                    Html.map MaterialMsg
                        (MaterialModule.view model.material model.materials model.units MaterialModule.IndexPage)

                MaterialShowRoute id ->
                    let
                        materialOrClearMaterial =
                            Maybe.withDefault Aliases.initMaterial (getMaterialByIdFromList model.materials id)
                    in
                        Html.map MaterialMsg
                            (MaterialModule.view model.material model.materials model.units (MaterialModule.ShowPage materialOrClearMaterial))

                MaterialNewRoute ->
                    Html.map MaterialMsg
                        (MaterialModule.view model.material model.materials model.units MaterialModule.NewPage)

                MaterialEditRoute id ->
                    let
                        materialOrClearMaterial =
                            Maybe.withDefault Aliases.initMaterial (getMaterialByIdFromList model.materials id)
                    in
                        Html.map MaterialMsg
                            (MaterialModule.view model.material model.materials model.units (MaterialModule.EditPage materialOrClearMaterial))

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
            , confirmModalView model
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
            UnitModule.subscriptions model.unit

        materialSub =
            MaterialModule.subscriptions model.material

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
