module Json.Schema.Form.Error exposing (ErrorValue(..), Errors)

{-| Validation errors.

@docs ErrorValue, Errors

-}

import Form exposing (Form)
import Form.Error


{-| A validation error. See `etaque/elm-form` for more error types.
-}
type ErrorValue
    = Invalid
    | InvalidSet
    | ShorterListThan Int
    | LongerListThan Int
    | InvalidCustomFormat String


{-| A function that converts a field path and an `ErrorValue` into a user readable string.
-}
type alias Errors =
    String -> Form.Error.ErrorValue ErrorValue -> String
