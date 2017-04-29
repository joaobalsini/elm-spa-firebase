module Store.Model exposing (..)

import Materials.Model exposing (Material)
import Units.Model exposing (Unit)
import Routes exposing (..)


type alias Model =
    { units : Maybe (List Unit)
    , materials : Maybe (List Material)
    , refreshRoute : Maybe Route
    }


initModel : Model
initModel =
    ({ units = Nothing
     , materials = Nothing
     , refreshRoute = Nothing
     }
    )
