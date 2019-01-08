module Json.Schema.Form.Format exposing (CustomFormat, Formats, getFormat)

import Dict
import Form.Validate exposing (Validation)
import Json.Schema.Form.Error exposing (ErrorValue)


type alias Formats =
    List ( String, CustomFormat )


type alias CustomFormat =
    { title : Maybe String
    , validation : String -> Validation ErrorValue String
    }


getFormat : String -> Formats -> Maybe CustomFormat
getFormat format formats =
    formats |> Dict.fromList |> Dict.get format
