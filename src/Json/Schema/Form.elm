module Json.Schema.Form exposing (Msg, Options, State, init, onSubmit, update, view)

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


type alias Options =
    { errors : Errors
    , formats : Formats
    }


type alias State =
    { options : Options
    , schema : Schema
    , form : F.Form ErrorValue Value
    }


type alias Msg =
    F.Msg


init : Options -> Schema -> State
init options schema =
    State options schema <|
        F.initial (default schema) (validation options.formats schema)


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
    Json.Schema.Form.Fields.schemaView state.options [] state.schema state.form


onSubmit : Attribute Msg
onSubmit =
    Html.Events.onSubmit F.Submit
