module Schema exposing (init, update, view)

import Form as F
import Html exposing (..)
import Json.Schema.Definitions exposing (Schema)
import Schema.Default exposing (default)
import Schema.Form exposing (Form)
import Schema.Validation exposing (validation)


type alias State =
    { form : Form
    , schema : Schema
    }


type alias Msg =
    F.Msg


init : Schema -> State
init schema =
    { form = F.initial (default schema) (validation schema)
    , schema = schema
    }


update : Msg -> State -> State
update msg state =
    { state | form = F.update (validation state.schema) msg state.form }


view : State -> Html Msg
view state =
    Schema.Form.schemaView [] state.schema state.form
