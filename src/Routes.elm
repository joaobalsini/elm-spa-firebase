module Routes exposing (..)


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
