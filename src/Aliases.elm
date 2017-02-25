module Aliases exposing (..)


type alias Unit =
    { id : String
    , name : String
    , initials : String
    }


initUnit : Unit
initUnit =
    { id = ""
    , name = ""
    , initials = ""
    }


type alias Message =
    { messageClass : String
    , header : String
    , text : String
    , active : Bool
    }


initMessage : Message
initMessage =
    { messageClass = ""
    , header = ""
    , text = ""
    , active = False
    }


errorMessage : String -> Message
errorMessage text =
    { initMessage
        | messageClass = "negative"
        , header = "Error"
        , text = text
        , active = True
    }


successMessage : String -> Message
successMessage text =
    { initMessage
        | messageClass = "positive"
        , header = "Success"
        , text = text
        , active = True
    }


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
