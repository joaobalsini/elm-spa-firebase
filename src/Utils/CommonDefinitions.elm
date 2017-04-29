module Utils.CommonDefinitions exposing (..)


type alias FieldError =
    { fieldName : String
    , errorMessage : Maybe String
    }
