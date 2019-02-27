# JSON Schema Form Generator

Generate validating forms from JSON schemas.

    elm package install niho/json-schema-form

## Features

* Can handle almost all JSON Schema features (draft-04 and draft-06).
* Generates all common types of input fields (`text`, `select`, etc.) with optional labels and descriptions.
* Error messages can easily be customized as needed.
* Supports custom string formats using validation functions (similar to Json decoders).


## Warnings

1. The way form fields are generated and presented is very opinionated and thus not always suitable for general case usage. This library is intended to be used for cases where you have control over how the schema is structured.
2. The HTML that the library outputs is intended to be used together with [Bootstrap](https://getbootstrap.com/) to style the form. It can of course be used without Bootstrap but some field types might need some custom styling to look ok.
3. There is currently no support for linked schemas using `$ref`.


## Example usage

See the [example project](https://github.com/niho/json-schema-form/tree/master/example) for examples of all the supported field types.

```elm
module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Events exposing (onSubmit)
import Json.Schema
import Json.Schema.Form exposing (Msg, State)


main : Program () State Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


schema =
  """
  {
    "type": "object",
    "required": ["name"],
    "properties": {
      "name": {
        "type": "string",
        "title": "Name"
      }
    }
  }
  """


init : State
init =
    case Json.Schema.fromString schema of
        Ok schema_ ->
            Json.Schema.Form.init
                { errors = \path error -> "Invalid field: " ++ path
                , formats = Dict.empty
                }
                schema_

        Err error ->
            Debug.todo error


update : Msg -> State -> State
update msg state =
    Json.Schema.Form.update msg state


view : State -> Html Msg
view state =
    form [ onSubmit Json.Schema.Form.submit ]
        [ Json.Schema.Form.view state
        , button [] [ text "Submit" ]
        ]
```
