module Routes exposing (..)

import UrlParser exposing (..)
import Navigation exposing (Location)
import Materials.Routes
import Units.Routes


type Route
    = IndexRoute
    | LoginRoute
    | UnitsRoutes Units.Routes.UnitRoute
    | MaterialsRoutes Materials.Routes.MaterialRoute
    | NotFoundRoute


routeToHash : Route -> String
routeToHash route =
    case route of
        IndexRoute ->
            "#/"

        LoginRoute ->
            "#/login"

        UnitsRoutes subroute ->
            "#/units/" ++ Units.Routes.unitRouteToHash subroute

        MaterialsRoutes subroute ->
            "#/materials/" ++ Materials.Routes.materialsRouteToHash subroute

        NotFoundRoute ->
            "#notfound"



-- URL PARSING


mainMatchers : List (Parser (Route -> a) a)
mainMatchers =
    [ map IndexRoute top
    , map LoginRoute (s "login")
    , map UnitsRoutes (s "units" </> (oneOf Units.Routes.unitMatchers))
    , map MaterialsRoutes (s "materials" </> (oneOf Materials.Routes.materialMatchers))
    ]


matchers : Parser (Route -> a) a
matchers =
    oneOf mainMatchers


locationToRoute : Location -> Route
locationToRoute location =
    case (parseHash matchers location) of
        Nothing ->
            NotFoundRoute

        Just route ->
            route
