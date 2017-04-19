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
    | ChangePage Route
    | IndexMsg Index.Msg
    | LoginMsg Login.Msg
    | UnitMsg Unit.Msg
    | MaterialMsg Material.Msg
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
                                            Unit.update (Unit.PrepareView (Unit.ShowPage (Maybe.withDefault Aliases.initUnit unit)) Aliases.initMessage) model.unit
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
                                            Unit.update (Unit.PrepareView (Unit.EditPage (Maybe.withDefault Aliases.initUnit unit)) Aliases.initMessage) model.unit
                                    in
                                        ( { model | route = route, lastRoute = lastRoute, unit = unitModel }, Cmd.none )
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

                -- in case its Updated we update the unit in the units list and pass the message to unit module
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

                -- in case its Removed we remove the unit from the units list and pass the message to unit module
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

                -- otherwise we just pass the message to unit module
                _ ->
                    let
                        ( unitModel, cmd, message ) =
                            Unit.update msg model.unit
                    in
                        ( { model | unit = unitModel, message = message }
                        , Cmd.map UnitMsg cmd
                        )

        MaterialMsg msg ->
            -- Here we handle the materials list, please check the units list above for comments
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
                        (Unit.view model.unit (Unit.IndexPage model.units))

                UnitShowRoute id ->
                    let
                        unit =
                            getUnitByIdFromList model.units id

                        unitOrClearUnit =
                            Maybe.withDefault Aliases.initUnit unit
                    in
                        Html.map UnitMsg
                            (Unit.view model.unit (Unit.ShowPage unitOrClearUnit))

                UnitNewRoute ->
                    Html.map UnitMsg
                        (Unit.view model.unit (Unit.NewPage))

                UnitEditRoute id ->
                    let
                        unit =
                            getUnitByIdFromList model.units id

                        unitOrClearUnit =
                            Maybe.withDefault Aliases.initUnit unit
                    in
                        Html.map UnitMsg
                            (Unit.view model.unit (Unit.EditPage unitOrClearUnit))

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
