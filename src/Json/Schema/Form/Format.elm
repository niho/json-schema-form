module Json.Schema.Form.Format exposing
    ( Format, init
    , withPrefix, withSuffix, withPlaceholder, withAutocompleteOff, withAutocompleteOn, withAutocomplete, withInputType, withLines
    , withValidation
    )

{-| In a JSON schema the `format` keyword has a number of pre-defined formats (`date`, `email`, etc.) but can also be any custom format (see [7. Semantic Validation with "format"](https://json-schema.org/latest/json-schema-validation.html#rfc.section.7)). If you simply need a to match a field against a regular expression you should use the `pattern` keyword instead. Custom formats are intended for annotation and more complex validation that is not possible to accomplish with a regex.


# Custom format

@docs Format, init


# Input field

@docs withPrefix, withSuffix, withPlaceholder, withAutocompleteOff, withAutocompleteOn, withAutocomplete, withInputType, withLines


# Validation

@docs withValidation

-}

import Form.Validate exposing (Validation)
import Json.Schema.Form.Error exposing (ErrorValue)


{-| A custom format.
-}
type alias Format =
    { prefix : Maybe String
    , suffix : Maybe String
    , placeholder : Maybe String
    , autocomplete : Maybe String
    , inputType : Maybe String
    , lines : Int
    , validation : String -> Validation ErrorValue String
    }


{-| Initialize a new format with default values.
-}
init : Format
init =
    { prefix = Nothing
    , suffix = Nothing
    , placeholder = Nothing
    , autocomplete = Nothing
    , inputType = Nothing
    , lines = 1
    , validation = Form.Validate.succeed
    }


{-| A short label that is displayed at the beginning of the input field.
-}
withPrefix : String -> Format -> Format
withPrefix str format =
    { format | prefix = Just str }


{-| A short label that is displayed at the end of the input field.
-}
withSuffix : String -> Format -> Format
withSuffix str format =
    { format | suffix = Just str }


{-| A short hint that describes the expected value of the field.
-}
withPlaceholder : String -> Format -> Format
withPlaceholder str format =
    { format | placeholder = Just str }


{-| The browser is not permitted to automatically enter or select a value for this field.
-}
withAutocompleteOff : Format -> Format
withAutocompleteOff format =
    { format | autocomplete = Just "off" }


{-| The browser is allowed to automatically complete the input. No guidance is provided as to the type of data expected in the field, so the browser may use its own judgement.
-}
withAutocompleteOn : Format -> Format
withAutocompleteOn format =
    { format | autocomplete = Just "on" }


{-| The browser is allowed to automatically complete the input. See <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete> for a list of possible values and <https://cloudfour.com/thinks/autofill-what-web-devs-should-know-but-dont/> for in-depth guidance on how to use autocomplete.
-}
withAutocomplete : String -> Format -> Format
withAutocomplete str format =
    { format | autocomplete = Just str }


{-| The type of `input` to use. The default is `text`.
-}
withInputType : String -> Format -> Format
withInputType str format =
    { format | inputType = Just str, lines = 1 }


{-| The expected number of lines. If more than one the field will be rendered as a `textarea`. The default is 1.
-}
withLines : Int -> Format -> Format
withLines lines format =
    { format | lines = Basics.max 0 lines }


{-| A validation function (see [etaque/elm-form](https://package.elm-lang.org/packages/etaque/elm-form/4.0.0/Form-Validate) for details on how to write a validation function).
-}
withValidation : (String -> Validation ErrorValue String) -> Format -> Format
withValidation validation format =
    { format | validation = validation }
