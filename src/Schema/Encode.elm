module Schema.Encode exposing (encode)

import Json.Encode exposing (..)
import Schema.Value exposing (Value(..))


encode : Schema.Value.Value -> Json.Encode.Value
encode value =
    case value of
        IntValue intValue ->
            int intValue

        FloatValue floatValue ->
            float floatValue

        StringValue stringValue ->
            string stringValue

        BoolValue boolValue ->
            bool boolValue

        ListValue valueList ->
            list encode valueList

        ObjectValue objectValue ->
            let
                item ( name, val ) =
                    ( name, encode val )
            in
            object (List.map item objectValue)

        EmptyValue ->
            null