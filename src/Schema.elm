module Schema exposing (Errors, Msg, Options, State, init, update, view)

import Form as F
import Html exposing (..)
import Json.Schema.Definitions exposing (Schema)
import Schema.Default exposing (default)
import Schema.Error
import Schema.Form exposing (Form, Options)
import Schema.Validation exposing (validation)


type alias Options =
    Schema.Form.Options


type alias Errors =
    Schema.Error.Errors Schema.Error.ValidationError


type alias State =
    { form : Form
    , schema : Schema
    , options : Options
    }


type alias Msg =
    F.Msg


init : Options -> Schema -> State
init options schema =
    { form = F.initial (default schema) (validation options.formats schema)
    , schema = schema
    , options = options
    }


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


view : State -> Html Msg
view state =
    Schema.Form.schemaView state.options [] state.schema state.form
