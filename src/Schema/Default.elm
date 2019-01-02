module Schema.Default exposing (default)

import Form.Field exposing (..)
import Json.Decode
import Json.Schema.Definitions
    exposing
        ( Items(..)
        , Schema(..)
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
                            anyType value
                                |> Maybe.map (\f -> [ ( name, f ) ])
                                |> Maybe.withDefault []

                        SingleType type_ ->
                            singleType schema_ value type_
                                |> Maybe.map (\f -> [ ( name, f ) ])
                                |> Maybe.withDefault []

                        NullableType type_ ->
                            singleType schema_ value type_
                                |> Maybe.map (\f -> [ ( name, f ) ])
                                |> Maybe.withDefault []

                        UnionType types ->
                            List.map (singleType schema_ value) types
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


anyType : Json.Decode.Value -> Maybe Field
anyType value =
    [ value |> Json.Decode.decodeValue asString |> Result.map string
    , value |> Json.Decode.decodeValue Json.Decode.bool |> Result.map bool
    , value
        |> Json.Decode.decodeValue asList
        |> Result.map (List.map string)
        |> Result.map list
    ]
        |> List.map Result.toMaybe
        |> List.filterMap identity
        |> List.head


singleType : SubSchema -> Json.Decode.Value -> SingleType -> Maybe Field
singleType schema value type_ =
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
            case schema.items of
                NoItems ->
                    value
                        |> Json.Decode.decodeValue (Json.Decode.list asString)
                        |> Result.toMaybe
                        |> Maybe.map (List.map string)
                        |> Maybe.map list

                ItemDefinition item ->
                    value
                        |> Json.Decode.decodeValue (Json.Decode.list asString)
                        |> Result.toMaybe
                        |> Maybe.map (List.map string)
                        |> Maybe.map list

                ArrayOfItems items ->
                    value
                        |> Json.Decode.decodeValue (Json.Decode.list asString)
                        |> Result.toMaybe
                        |> Maybe.map
                            (List.indexedMap
                                (\idx str ->
                                    ( "tuple" ++ String.fromInt idx
                                    , string str
                                    )
                                )
                            )
                        |> Maybe.map group

        ObjectType ->
            Nothing

        NullType ->
            Nothing


asString : Json.Decode.Decoder String
asString =
    Json.Decode.oneOf
        [ Json.Decode.string
        , Json.Decode.int
            |> Json.Decode.andThen
                (\a ->
                    Json.Decode.succeed (String.fromInt a)
                )
        , Json.Decode.float
            |> Json.Decode.andThen
                (\a ->
                    Json.Decode.succeed (String.fromFloat a)
                )
        ]


asList : Json.Decode.Decoder (List String)
asList =
    Json.Decode.list asString
