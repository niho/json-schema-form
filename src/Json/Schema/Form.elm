module Json.Schema.Form exposing
    ( Options, State, Msg
    , init, update
    , view, onSubmit
    , getOutput
    )

{-| Generate validating forms from JSON schemas.


# Types

@docs Options, State, Msg


# Init/update lifecycle

@docs init, update


# View

@docs view, onSubmit

-}

import Form as F exposing (Msg(..))
import Html exposing (..)
import Html.Events
import Json.Schema.Definitions exposing (Schema)
import Json.Schema.Form.Default exposing (default)
import Json.Schema.Form.Encode
import Json.Schema.Form.Error exposing (ErrorValue, Errors)
import Json.Schema.Form.Fields
import Json.Schema.Form.Format exposing (Formats)
import Json.Schema.Form.Validation exposing (validation)
import Json.Schema.Form.Value exposing (Value)


{-| Customize the generated form.

  - `errors` - A function that turns error values into user readable strings.
  - `formats` - A list of custom formats (see `Json.Schema.Form.Format`).

-}
type alias Options =
    { errors : Errors
    , formats : Formats
    }


{-| The form state.
-}
type alias State =
    { options : Options
    , schema : Schema
    , form : F.Form ErrorValue Value
    }


{-| Form messages for `update`.
-}
type alias Msg =
    F.Msg


{-| Initialize a form state with options and a schema. Use [json-tools/json-schema](https://package.elm-lang.org/packages/json-tools/json-schema/1.0.2/) to parse or build a schema.
-}
init : Options -> Schema -> State
init options schema =
    State options schema <|
        F.initial (default schema) (validation options.formats schema)


{-| Update the form state.
-}
update : Msg -> State -> State
update msg state =
    let
        form =
            F.update
                (validation state.options.formats state.schema)
                msg
                state.form
    in
    { state | form = form }


{-| The form fields as HTML. Use together with `onSubmit` to submit the form.
-}
view : State -> Html Msg
view state =
    Json.Schema.Form.Fields.schemaView state.options [] state.schema state.form


{-| Triggers the form to be submitted for validation.

    form [ Json.Schema.Form.onSubmit ]
        [ Json.Schema.Form.view state
        , button [] [ text "Submit" ]
        ]

-}
onSubmit : Attribute Msg
onSubmit =
    Html.Events.onSubmit F.Submit


{-| Get the output value of the form if it validates.

    case Json.Schema.Form.getOutput state of
        Just value ->
            -- The `value` is a tree of all the fields in the form (see the
            -- `Json.Schema.Form.Value` module).
            -- Use the `Json.Schema.Form.Encode.encode` function to turn
            -- it into a JSON value.
        Nothing ->
            -- If any field in the form is not valid `getOutput` will
            -- return `Nothing`.

-}
getOutput : State -> Maybe Value
getOutput state =
    F.getOutput state.form
