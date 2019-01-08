module Json.Schema.Form.Error exposing (ErrorValue(..), Errors)

import Form exposing (Form)
import Form.Error


type ErrorValue
    = Invalid
    | InvalidSet
    | ShorterListThan Int
    | LongerListThan Int
    | InvalidCustomFormat String


type alias Errors =
    String -> Form.Error.ErrorValue ErrorValue -> String
