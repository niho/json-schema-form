module Json.Schema.Form.Encode exposing (encode)

{-| Encode form values as JSON.


# Encode

@docs encode

-}

import Json.Encode exposing (..)
import Json.Schema.Form.Value exposing (Value(..))


{-| Encode a form value (the output of a valid form) as JSON.
-}
encode : Value -> Json.Encode.Value
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
                    if val == EmptyValue then
                        Nothing

                    else
                        Just ( name, encode val )
            in
            object (List.filterMap item objectValue)

        NullValue ->
            null

        EmptyValue ->
            object []

        JsonValue jsonValue ->
            jsonValue
