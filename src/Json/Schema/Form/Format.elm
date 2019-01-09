module Json.Schema.Form.Format exposing (Formats, CustomFormat)

{-| In a JSON schema the `format` keyword has a number of pre-defined formats (`date`, `email`, etc.) but can also be any custom format (see [7. Semantic Validation with "format"](https://json-schema.org/latest/json-schema-validation.html#rfc.section.7)). If you simply need a to match a field against a regular expression you should use the `pattern` keyword instead. Custom formats are intended for annotation and more complex validation that is not possible to accomplish with a regex.

@docs Formats, CustomFormat

-}

import Form.Validate exposing (Validation)
import Json.Schema.Form.Error exposing (ErrorValue)


{-| A list of custom formats with a format identifier for each.
-}
type alias Formats =
    List ( String, CustomFormat )


{-| A custom format has a title and a validation function that validates the string value of the field according to the rules of the format.

  - `title` - A user readable description of the format that is used to annotate the field.
  - `validation` - A validation function (see [etaque/elm-form](https://package.elm-lang.org/packages/etaque/elm-form/4.0.0/Form-Validate) for details on how to write a validation function).

-}
type alias CustomFormat =
    { title : Maybe String
    , validation : String -> Validation ErrorValue String
    }
