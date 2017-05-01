module Utils.CommonDefinitions exposing (..)


type alias FieldError =
    { fieldName : String
    , errorNotification : Maybe String
    }
