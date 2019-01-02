module Schema.Default exposing (default)

import Form.Field exposing (..)
import Json.Decode
import Json.Schema.Definitions
    exposing
        ( Schema(..)
        , Schemata(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        )


default : Schema -> List ( String, Field )
default schema =
    case schema of
        BooleanSchema bool ->
            if bool then
                []

            else
                []

        ObjectSchema objectSchema ->
            subSchema objectSchema


subSchema : SubSchema -> List ( String, Field )
subSchema schema =
    case schema.properties of
        Just (Schemata props) ->
            List.concatMap field props

        Nothing ->
            []


field : ( String, Schema ) -> List ( String, Field )
field ( name, schema ) =
    case schema of
        BooleanSchema _ ->
            []

        ObjectSchema schema_ ->
            case schema_.default of
                Just value ->
                    case schema_.type_ of
                        AnyType ->
                            []

                        SingleType type_ ->
                            singleType value type_
                                |> Maybe.map (\f -> [ ( name, f ) ])
                                |> Maybe.withDefault []

                        NullableType type_ ->
                            singleType value type_
                                |> Maybe.map (\f -> [ ( name, f ) ])
                                |> Maybe.withDefault []

                        UnionType types ->
                            List.map (singleType value) types
                                |> List.filterMap identity
                                |> List.map (\f -> ( name, f ))

                Nothing ->
                    subSchema schema_
                        |> (\fields ->
                                case fields of
                                    [] ->
                                        []

                                    _ ->
                                        [ ( name, group fields ) ]
                           )


singleType : Json.Decode.Value -> SingleType -> Maybe Field
singleType value type_ =
    case type_ of
        IntegerType ->
            value
                |> Json.Decode.decodeValue Json.Decode.int
                |> Result.toMaybe
                |> Maybe.map String.fromInt
                |> Maybe.map string

        NumberType ->
            value
                |> Json.Decode.decodeValue Json.Decode.float
                |> Result.toMaybe
                |> Maybe.map String.fromFloat
                |> Maybe.map string

        StringType ->
            value
                |> Json.Decode.decodeValue Json.Decode.string
                |> Result.toMaybe
                |> Maybe.map string

        BooleanType ->
            value
                |> Json.Decode.decodeValue Json.Decode.bool
                |> Result.toMaybe
                |> Maybe.map bool

        ArrayType ->
            Nothing

        ObjectType ->
            Nothing

        NullType ->
            Nothing
