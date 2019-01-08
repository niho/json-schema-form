module Json.Schema.Form.Value exposing (Value(..))


type Value
    = IntValue Int
    | FloatValue Float
    | StringValue String
    | BoolValue Bool
    | ListValue (List Value)
    | ObjectValue (List ( String, Value ))
    | NullValue
    | EmptyValue
