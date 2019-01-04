module Schema.Validation exposing (Formats, getFormat, validation)

import Dict
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


type alias Formats =
    List ( String, CustomFormat )


type alias CustomFormat =
    { title : Maybe String
    , validation : String -> Validation ValidationError String
    }


validation : Formats -> Schema -> Validation ValidationError Value
validation formats schema =
    case schema of
        BooleanSchema bool ->
            if bool then
                succeed EmptyValue

            else
                fail (customError Invalid)

        ObjectSchema objectSchema ->
            subSchema formats objectSchema


subSchema : Formats -> SubSchema -> Validation ValidationError Value
subSchema formats schema =
    case schema.type_ of
        AnyType ->
            oneOf
                [ singleType formats schema IntegerType
                , singleType formats schema NumberType
                , singleType formats schema StringType
                , singleType formats schema BooleanType
                , singleType formats schema ArrayType
                , singleType formats schema ObjectType
                , singleType formats schema NullType
                ]

        NullableType type_ ->
            oneOf
                [ singleType formats schema type_
                , emptyString |> andThen (\_ -> succeed NullValue)
                ]

        UnionType types ->
            oneOf (List.map (singleType formats schema) types)

        SingleType type_ ->
            singleType formats schema type_


singleType : Formats -> SubSchema -> SingleType -> Validation ValidationError Value
singleType formats schema type_ =
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
                |> andMaybe (customFormat formats) schema.format
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
                    list (validation formats schema_)
                        |> andMaybe uniqueItems schema.uniqueItems
                        |> andMaybe minItems schema.minItems
                        |> andMaybe maxItems schema.maxItems
                        |> map ListValue

                ArrayOfItems schemaList ->
                    tuple (List.map (validation formats) schemaList)
                        |> andMaybe uniqueItems schema.uniqueItems
                        |> andMaybe minItems schema.minItems
                        |> andMaybe maxItems schema.maxItems
                        |> map ListValue

        ObjectType ->
            let
                required =
                    schema.required |> Maybe.withDefault []

                isSpecialType =
                    isType [ BooleanType, ArrayType, ObjectType ]

                schemataItem ( name, schema_ ) =
                    if List.member name required || isSpecialType schema_ then
                        field name (validation formats schema_)
                            |> andThen (\v -> succeed ( name, v ))

                    else
                        oneOf
                            [ field name emptyString
                                |> andThen (\_ -> succeed ( name, EmptyValue ))
                            , field name (validation formats schema_)
                                |> andThen (\v -> succeed ( name, v ))
                            ]

                fields =
                    case schema.properties of
                        Nothing ->
                            []

                        Just (Json.Schema.Definitions.Schemata schemata) ->
                            List.map schemataItem schemata
            in
            sequence fields |> map ObjectValue

        NullType ->
            emptyString |> andThen (\_ -> succeed NullValue)


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


customFormat : Formats -> String -> String -> Validation ValidationError String
customFormat formats formatId value =
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

        format ->
            Dict.fromList formats
                |> Dict.get format
                |> Maybe.map .validation
                |> Maybe.map
                    (\v ->
                        v value
                            |> withCustomError
                                (InvalidCustomFormat format)
                    )
                |> Maybe.withDefault (succeed value)


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



-- required : String -> List String -> Value -> Validation ValidationError Value
-- required name fields value =
--     if List.member name fields then
--         \f -> Ok (Result.withDefault EmptyValue (validation validationField))
--
--     else
--         succeed value


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


getFormat : String -> Formats -> Maybe CustomFormat
getFormat format formats =
    formats |> Dict.fromList |> Dict.get format


isType : List SingleType -> Schema -> Bool
isType types schema_ =
    List.any
        (\t ->
            case schema_ of
                ObjectSchema s ->
                    case s.type_ of
                        AnyType ->
                            BooleanType == t

                        NullableType type_ ->
                            type_ == t

                        UnionType _ ->
                            StringType == t

                        SingleType type_ ->
                            type_ == t

                _ ->
                    False
        )
        types
