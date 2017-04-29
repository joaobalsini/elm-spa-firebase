module Units.Routes exposing (..)

import UrlParser exposing (..)


type UnitRoute
    = UnitIndexRoute
    | UnitShowRoute String
    | UnitNewRoute
    | UnitEditRoute String


unitMatchers : List (Parser (UnitRoute -> a) a)
unitMatchers =
    [ map UnitIndexRoute top
    , map UnitEditRoute (s "edit" </> string)
    , map UnitNewRoute (s "new")
    , map UnitShowRoute (string)
    ]


unitRouteToHash : UnitRoute -> String
unitRouteToHash route =
    case route of
        UnitIndexRoute ->
            ""

        UnitNewRoute ->
            "new"

        UnitShowRoute id ->
            "" ++ id

        UnitEditRoute id ->
            "edit/" ++ id
