module Json.Schema.Form.Value exposing (Value(..))

{-| When a form is valid the output is a tree of `Value` items that represent the type and value of each field or group/list of fields in the form.

@docs Value

-}


{-| A form value.
-}
type Value
    = IntValue Int
    | FloatValue Float
    | StringValue String
    | BoolValue Bool
    | ListValue (List Value)
    | ObjectValue (List ( String, Value ))
    | NullValue
    | EmptyValue
