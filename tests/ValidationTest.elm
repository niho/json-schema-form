module ValidationTest exposing (suite)

import Expect exposing (Expectation)
import Form.Field exposing (..)
import Json.Schema.Builder exposing (..)
import Json.Schema.Definitions exposing (..)
import Json.Schema.Form.Validation exposing (validation)
import Json.Schema.Form.Value exposing (Value(..))
import Test exposing (..)


suite : Test
suite =
    describe "Json.Schema.Form.Validation"
        [ describe "validation"
            [ describe "with a blank schema"
                [ describe "any type of input"
                    (Ok blankSchema
                        |> validateMultiple
                            [ ( "empty string", string "", Expect.ok )
                            , ( "number", string "3.14", Expect.ok )
                            , ( "integer", string "42", Expect.ok )
                            , ( "boolean", bool True, Expect.ok )
                            , ( "array", list [], Expect.ok )
                            , ( "object", group [], Expect.ok )
                            ]
                    )
                ]
            , describe "with a boolean schema"
                [ describe "true"
                    [ test "should always validate" <|
                        \_ ->
                            (boolSchema True |> toSchema)
                                |> Result.andThen (validate (string "test"))
                                |> Expect.equal (Ok (StringValue "test"))
                    ]
                , describe "false"
                    [ test "should never validate" <|
                        \_ ->
                            (boolSchema False |> toSchema)
                                |> Result.andThen (validate (string "test"))
                                |> Expect.err
                    ]
                ]
            , describe "with an object schema"
                [ describe "any type"
                    ((buildSchema |> toSchema)
                        |> validateMultiple
                            [ ( "empty string"
                              , string ""
                              , Expect.equal (Ok (BoolValue False))
                              )
                            , ( "string"
                              , string "test"
                              , Expect.equal (Ok (StringValue "test"))
                              )
                            , ( "number"
                              , string "3.14"
                              , Expect.equal (Ok (FloatValue 3.14))
                              )
                            , ( "integer"
                              , string "42"
                              , Expect.equal (Ok (IntValue 42))
                              )
                            , ( "boolean true"
                              , bool True
                              , Expect.equal (Ok (BoolValue True))
                              )
                            , ( "boolean false"
                              , bool False
                              , Expect.equal (Ok (BoolValue False))
                              )
                            , ( "empty array"
                              , list []
                              , Expect.equal (Ok (BoolValue False))
                              )
                            , ( "array"
                              , list [ string "test" ]
                              , Expect.equal
                                    (Ok
                                        (ListValue
                                            [ StringValue "test"
                                            ]
                                        )
                                    )
                              )
                            , ( "empty object"
                              , group []
                              , Expect.equal (Ok (BoolValue False))
                              )
                            , ( "object"
                              , group [ ( "test", string "test" ) ]
                              , Expect.equal (Ok (BoolValue False))
                              )
                            ]
                    )
                , describe "nullable type"
                    [ describe "with empty string"
                        [ test "should succeed with NullValue" <|
                            \_ ->
                                (buildSchema
                                    |> withNullableType "string"
                                    |> toSchema
                                )
                                    |> Result.andThen (validate (string ""))
                                    |> Expect.equal (Ok NullValue)
                        ]
                    ]
                , describe "union type"
                    [ describe "with all types"
                        ((buildSchema
                            |> withUnionType
                                [ "integer"
                                , "number"
                                , "string"
                                , "boolean"
                                , "array"
                                , "object"
                                ]
                            |> toSchema
                         )
                            |> validateMultiple
                                [ ( "empty string", string "", Expect.ok )
                                , ( "number", string "3.14", Expect.ok )
                                , ( "integer", string "42", Expect.ok )
                                , ( "boolean", bool True, Expect.ok )
                                , ( "array", list [], Expect.ok )
                                , ( "object", group [], Expect.ok )
                                ]
                        )
                    ]
                , describe "single type" singleTypes
                ]
            ]
        ]


singleTypes =
    [ describe "integer"
        ((buildSchema
            |> withType "integer"
            |> toSchema
         )
            |> validateMultiple
                [ ( "with empty string", string "", Expect.err )
                , ( "with number", string "3.14", Expect.err )
                , ( "with integer", string "42", Expect.ok )
                , ( "with boolean", bool True, Expect.err )
                , ( "with array", list [], Expect.err )
                , ( "with object", group [], Expect.err )
                ]
        )
    , describe "number"
        ((buildSchema
            |> withType "number"
            |> toSchema
         )
            |> validateMultiple
                [ ( "with empty string", string "", Expect.err )
                , ( "with number", string "3.14", Expect.ok )
                , ( "with integer", string "42", Expect.ok )
                , ( "with boolean", bool True, Expect.err )
                , ( "with array", list [], Expect.err )
                , ( "with object", group [], Expect.err )
                ]
        )
    , describe "string"
        ((buildSchema
            |> withType "string"
            |> toSchema
         )
            |> validateMultiple
                [ ( "with empty string", string "", Expect.err )
                , ( "with number", string "3.14", Expect.ok )
                , ( "with integer", string "42", Expect.ok )
                , ( "with boolean", bool True, Expect.err )
                , ( "with array", list [], Expect.err )
                , ( "with object", group [], Expect.err )
                ]
        )
    , describe "boolean"
        ((buildSchema
            |> withType "boolean"
            |> toSchema
         )
            |> validateMultiple
                [ ( "with empty string"
                  , string ""
                  , Expect.equal (Ok (BoolValue False))
                  )
                , ( "with number"
                  , string "3.14"
                  , Expect.equal (Ok (BoolValue False))
                  )
                , ( "with integer"
                  , string "42"
                  , Expect.equal (Ok (BoolValue False))
                  )
                , ( "with boolean true"
                  , bool True
                  , Expect.equal (Ok (BoolValue True))
                  )
                , ( "with boolean false"
                  , bool False
                  , Expect.equal (Ok (BoolValue False))
                  )
                , ( "with array"
                  , list []
                  , Expect.equal (Ok (BoolValue False))
                  )
                , ( "with object"
                  , group []
                  , Expect.equal (Ok (BoolValue False))
                  )
                ]
        )
    , describe "array"
        ((buildSchema
            |> withType "array"
            |> withItem buildSchema
            |> toSchema
         )
            |> validateMultiple
                [ ( "with empty string"
                  , string ""
                  , Expect.equal (Ok (ListValue []))
                  )
                , ( "with number"
                  , string "3.14"
                  , Expect.equal (Ok (ListValue []))
                  )
                , ( "with integer"
                  , string "42"
                  , Expect.equal (Ok (ListValue []))
                  )
                , ( "with boolean"
                  , bool True
                  , Expect.equal (Ok (ListValue []))
                  )
                , ( "with empty array"
                  , list []
                  , Expect.equal (Ok (ListValue []))
                  )
                , ( "with array"
                  , list [ string "test" ]
                  , Expect.equal
                        (Ok
                            (ListValue
                                [ StringValue "test" ]
                            )
                        )
                  )
                , ( "with nested array"
                  , list [ list [ string "test" ] ]
                  , Expect.equal
                        (Ok
                            (ListValue
                                [ ListValue
                                    [ StringValue "test" ]
                                ]
                            )
                        )
                  )
                , ( "with object"
                  , group []
                  , Expect.equal (Ok (ListValue []))
                  )
                ]
        )
    , describe "object"
        ((buildSchema
            |> withType "object"
            |> withProperties [ ( "test", buildSchema ) ]
            |> toSchema
         )
            |> validateMultiple
                [ ( "with empty string"
                  , string ""
                  , Expect.equal
                        (Ok
                            (ObjectValue
                                [ ( "test", BoolValue False ) ]
                            )
                        )
                  )
                , ( "with number"
                  , string "3.14"
                  , Expect.equal
                        (Ok
                            (ObjectValue
                                [ ( "test", BoolValue False ) ]
                            )
                        )
                  )
                , ( "with integer"
                  , string "42"
                  , Expect.equal
                        (Ok
                            (ObjectValue
                                [ ( "test", BoolValue False ) ]
                            )
                        )
                  )
                , ( "with boolean"
                  , bool True
                  , Expect.equal
                        (Ok
                            (ObjectValue
                                [ ( "test", BoolValue False ) ]
                            )
                        )
                  )
                , ( "with array"
                  , list []
                  , Expect.equal
                        (Ok
                            (ObjectValue
                                [ ( "test", BoolValue False ) ]
                            )
                        )
                  )
                , ( "with empty object"
                  , group []
                  , Expect.equal
                        (Ok
                            (ObjectValue
                                [ ( "test", BoolValue False ) ]
                            )
                        )
                  )
                , ( "with object"
                  , group [ ( "test", string "test" ) ]
                  , Expect.equal
                        (Ok
                            (ObjectValue
                                [ ( "test"
                                  , StringValue "test"
                                  )
                                ]
                            )
                        )
                  )
                , ( "with nested object"
                  , group [ ( "test", group [ ( "test", string "test" ) ] ) ]
                  , Expect.equal
                        (Ok
                            (ObjectValue
                                [ ( "test", BoolValue False ) ]
                            )
                        )
                  )
                ]
        )
    , describe "null"
        ((buildSchema
            |> withType "null"
            |> toSchema
         )
            |> validateMultiple
                [ ( "with empty string"
                  , string ""
                  , Expect.equal (Ok NullValue)
                  )
                , ( "with number", string "3.14", Expect.err )
                , ( "with integer", string "42", Expect.err )
                , ( "with boolean", bool True, Expect.ok )
                , ( "with array", list [], Expect.ok )
                , ( "with object", group [], Expect.ok )
                ]
        )
    ]


validate field schema =
    validation [] schema field
        |> Result.mapError Debug.toString


validateMultiple fields schema =
    List.map
        (\( name, f, expect ) ->
            test name
                (\_ ->
                    schema
                        |> Result.andThen (validate f)
                        |> expect
                )
        )
        fields
