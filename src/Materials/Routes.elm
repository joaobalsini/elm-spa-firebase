module Materials.Routes exposing (..)

import UrlParser exposing (..)


type MaterialRoute
    = MaterialIndexRoute
    | MaterialShowRoute String
    | MaterialNewRoute
    | MaterialEditRoute String


materialsRouteToHash : MaterialRoute -> String
materialsRouteToHash route =
    case route of
        MaterialIndexRoute ->
            ""

        MaterialNewRoute ->
            "new"

        MaterialShowRoute id ->
            "" ++ id

        MaterialEditRoute id ->
            "edit/" ++ id


materialMatchers : List (Parser (MaterialRoute -> a) a)
materialMatchers =
    [ map MaterialEditRoute (s "edit" </> string)
    , map MaterialNewRoute (s "new")
    , map MaterialIndexRoute top
    , map MaterialShowRoute (string)
    ]
