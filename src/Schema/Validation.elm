module Schema.Validation exposing (validation)

import Form.Error exposing (ErrorValue(..))
import Form.Field exposing (Field)
import Form.Validate exposing (..)
import Json.Encode
import Json.Schema.Definitions
    exposing
        ( ExclusiveBoundary(..)
        , Items(..)
        , Schema(..)
        , Schemata
        , SingleType(..)
        , SubSchema
        , Type(..)
        )
import Regex
import Schema.Encode
import Schema.Error exposing (ValidationError(..))
import Schema.Format
import Schema.Value exposing (Value(..))
import Set


validation : Schema -> Validation ValidationError Value
validation schema =
    case schema of
        BooleanSchema bool ->
            if bool then
                succeed EmptyValue

            else
                fail (customError Invalid)

        ObjectSchema objectSchema ->
            subSchema objectSchema


subSchema : SubSchema -> Validation ValidationError Value
subSchema schema =
    case schema.type_ of
        AnyType ->
            oneOf
                [ singleType schema IntegerType
                , singleType schema NumberType
                , singleType schema StringType
                , singleType schema BooleanType
                , singleType schema ArrayType
                , singleType schema ObjectType
                , singleType schema NullType
                ]

        NullableType type_ ->
            oneOf
                [ singleType schema type_
                , emptyString |> andThen (\_ -> succeed EmptyValue)
                ]

        UnionType types ->
            oneOf (List.map (singleType schema) types)

        SingleType type_ ->
            singleType schema type_


singleType : SubSchema -> SingleType -> Validation ValidationError Value
singleType schema type_ =
    case type_ of
        IntegerType ->
            int
                |> andMaybe constInt schema.const
                |> andMaybe multipleOf (Maybe.map round schema.multipleOf)
                |> andMaybe minInt (Maybe.map round (minimum schema))
                |> andMaybe maxInt (Maybe.map round (maximum schema))
                |> andMaybe enumInt schema.enum
                |> map IntValue

        NumberType ->
            float
                |> andMaybe constFloat schema.const
                |> andMaybe minFloat (minimum schema)
                |> andMaybe maxFloat (maximum schema)
                |> andMaybe enumFloat schema.enum
                |> map FloatValue

        StringType ->
            string
                |> andMaybe constString schema.const
                |> andMaybe minLength schema.minLength
                |> andMaybe maxLength schema.maxLength
                |> andMaybe pattern schema.pattern
                |> andMaybe enumString schema.enum
                |> andMaybe customFormat schema.format
                |> map StringValue

        BooleanType ->
            bool
                |> andMaybe constBool schema.const
                |> map BoolValue

        ArrayType ->
            case schema.items of
                NoItems ->
                    list (fail (customError Invalid))
                        |> map ListValue

                ItemDefinition schema_ ->
                    list (validation schema_)
                        |> andMaybe uniqueItems schema.uniqueItems
                        |> andMaybe minItems schema.minItems
                        |> andMaybe maxItems schema.maxItems
                        |> map ListValue

                ArrayOfItems schemaList ->
                    tuple (List.map validation schemaList)
                        |> andMaybe uniqueItems schema.uniqueItems
                        |> andMaybe minItems schema.minItems
                        |> andMaybe maxItems schema.maxItems
                        |> map ListValue

        ObjectType ->
            let
                schemataItem ( name, schema_ ) =
                    field name (validation schema_)
                        |> andThen (\v -> succeed ( name, v ))

                fields =
                    case schema.properties of
                        Nothing ->
                            []

                        Just (Json.Schema.Definitions.Schemata schemata) ->
                            List.map schemataItem schemata
            in
            sequence fields |> map ObjectValue

        NullType ->
            emptyString |> andThen (\_ -> succeed EmptyValue)


constInt : Json.Encode.Value -> Int -> Validation ValidationError Int
constInt constValue value =
    if Json.Encode.int value == constValue then
        succeed value

    else
        fail (Form.Error.value InvalidInt)


constFloat : Json.Encode.Value -> Float -> Validation ValidationError Float
constFloat constValue value =
    if Json.Encode.float value == constValue then
        succeed value

    else
        fail (Form.Error.value InvalidFloat)


constString : Json.Encode.Value -> String -> Validation ValidationError String
constString constValue value =
    if Json.Encode.string value == constValue then
        succeed value

    else
        fail (Form.Error.value InvalidString)


constBool : Json.Encode.Value -> Bool -> Validation ValidationError Bool
constBool constValue value =
    if Json.Encode.bool value == constValue then
        succeed value

    else
        fail (Form.Error.value InvalidBool)


pattern : String -> (String -> Validation e String)
pattern str =
    case Regex.fromString str of
        Just regex ->
            format regex

        Nothing ->
            \_ -> fail (Form.Error.value InvalidFormat)


multipleOf : Int -> Int -> Validation e Int
multipleOf multiplier value =
    if remainderBy multiplier value == 0 then
        succeed value

    else
        fail (Form.Error.value NotIncludedIn)


minimum : SubSchema -> Maybe Float
minimum schema =
    case schema.exclusiveMinimum of
        Just (BoolBoundary True) ->
            Maybe.map (\value -> value + 1) schema.minimum

        Just (BoolBoundary False) ->
            schema.minimum

        Just (NumberBoundary value) ->
            Just (value + 1)

        Nothing ->
            schema.minimum


maximum : SubSchema -> Maybe Float
maximum schema =
    case schema.exclusiveMaximum of
        Just (BoolBoundary True) ->
            Maybe.map (\value -> value - 1) schema.maximum

        Just (BoolBoundary False) ->
            schema.maximum

        Just (NumberBoundary value) ->
            Just (value - 1)

        Nothing ->
            schema.maximum


enumInt : List Json.Encode.Value -> Int -> Validation ValidationError Int
enumInt =
    enum Json.Encode.int


enumFloat : List Json.Encode.Value -> Float -> Validation ValidationError Float
enumFloat =
    enum Json.Encode.float


enumString : List Json.Encode.Value -> String -> Validation ValidationError String
enumString =
    enum Json.Encode.string


enum :
    (a -> Json.Encode.Value)
    -> List Json.Encode.Value
    -> a
    -> Validation ValidationError a
enum encode constValues value =
    if List.member (encode value) constValues then
        succeed value

    else
        fail (Form.Error.value NotIncludedIn)


customFormat : String -> String -> Validation ValidationError String
customFormat formatId value =
    case formatId of
        "date-time" ->
            format Schema.Format.dateTime value

        "date" ->
            format Schema.Format.date value

        "time" ->
            format Schema.Format.time value

        "email" ->
            format Schema.Format.email value

        "hostname" ->
            format Schema.Format.hostname value

        "ipv4" ->
            format Schema.Format.ipv4 value

        "ipv6" ->
            format Schema.Format.ipv6 value

        _ ->
            succeed value


uniqueItems : Bool -> List Value -> Validation ValidationError (List Value)
uniqueItems unique value =
    let
        items =
            List.map Schema.Encode.encode value
                |> List.map (Json.Encode.encode 0)
    in
    if unique then
        if Set.size (Set.fromList items) == List.length value then
            succeed value

        else
            fail (customError InvalidSet)

    else
        succeed value


minItems : Int -> List a -> Validation ValidationError (List a)
minItems count list =
    if List.length list >= count then
        succeed list

    else
        fail (customError (ShorterListThan count))


maxItems : Int -> List a -> Validation ValidationError (List a)
maxItems count list =
    if List.length list <= count then
        succeed list

    else
        fail (customError (LongerListThan count))


tuple : List (Validation ValidationError a) -> Validation ValidationError (List a)
tuple validations =
    let
        item idx =
            field ("tuple" ++ String.fromInt idx)
    in
    List.indexedMap item validations
        |> sequence


andMaybe :
    (a -> b -> Validation ValidationError b)
    -> Maybe a
    -> (Validation ValidationError b -> Validation ValidationError b)
andMaybe func constraint =
    case constraint of
        Just constraintValue ->
            andThen (\value -> func constraintValue value)

        Nothing ->
            andThen (\value -> succeed value)
