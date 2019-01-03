module Main exposing (main)

import Browser
import Form exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Json.Encode exposing (bool, float, int, list, string)
import Json.Schema.Builder exposing (..)
import Json.Schema.Definitions
import Schema
import Schema.Encode


main : Program () Schema.State Schema.Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


schema : Result String Json.Schema.Definitions.Schema
schema =
    buildSchema
        |> withId "http://example.com/schema#"
        |> withTitle "My object"
        |> withDescription "This is an example form."
        |> withType "object"
        |> withProperties
            [ ( "name"
              , buildSchema
                    |> withType "string"
                    |> withTitle "Name"
                    |> withDescription "Please enter your name."
                    |> withMaxLength 10
                    |> withMinLength 2
              )
            , ( "int"
              , buildSchema
                    |> withType "integer"
                    |> withTitle "Integer"
                    |> withExclusiveMinimum 5
                    |> withExclusiveMaximum 10
                    |> withMultipleOf 2
                    |> withDefault (int 6)
              )
            , ( "float"
              , buildSchema
                    |> withType "number"
                    |> withTitle "Number"
                    |> withMinimum 5.5
                    |> withMaximum 10.1
                    |> withDefault (float 6.8)
              )
            , ( "color"
              , buildSchema
                    |> withTitle "Color"
                    |> withDescription "Please enter either red, gren or blue."
                    |> withNullableType "string"
                    |> withEnum
                        [ string "red"
                        , string "green"
                        , string "blue"
                        ]
              )
            , ( "address"
              , buildSchema
                    |> withType "array"
                    |> withDefault
                        (list string
                            [ "1600"
                            , "Pennsylvania"
                            , "Avenue"
                            , "NW"
                            ]
                        )
                    |> withItems
                        [ buildSchema
                            |> withType "integer"
                            |> withTitle "Street number"
                        , buildSchema
                            |> withType "string"
                            |> withTitle "Street name"
                        , buildSchema
                            |> withType "string"
                            |> withTitle "Street type"
                            |> withOneOf
                                [ boolSchema False
                                , buildSchema |> withConst (string "Street")
                                , buildSchema |> withConst (string "Avenue")
                                , buildSchema |> withConst (string "Boulevard")
                                ]
                        , buildSchema
                            |> withType "string"
                            |> withTitle "Direction"
                            |> withEnum
                                [ string "NW"
                                , string "NE"
                                , string "SW"
                                , string "SE"
                                ]
                        ]
              )
            , ( "airports"
              , buildSchema
                    |> withTitle "Add airport"
                    |> withType "array"
                    |> withDefault (list string [ "LHR", "CDG" ])
                    |> withUniqueItems True
                    |> withMinItems 2
                    |> withItem
                        (buildSchema
                            |> withType "string"
                            |> withAnyOf
                                [ buildSchema
                                    |> withTitle "Stockholm Arlanda"
                                    |> withConst (string "ARN")
                                , buildSchema
                                    |> withTitle "London Heathrow"
                                    |> withConst (string "LHR")
                                    |> withDescription "Heathrow Airport is a major international airport in London, United Kingdom."
                                , buildSchema
                                    |> withTitle "Dubai International Airport"
                                    |> withConst (string "DXB")
                                , buildSchema
                                    |> withTitle "Paris Charles de Gaulle"
                                    |> withDescription "Paris Charles de Gaulle Airport is the largest international airport in France and the second largest in Europe."
                                    |> withConst (string "CDG")
                                ]
                        )
              )
            , ( "social"
              , buildSchema
                    |> withType "object"
                    |> withTitle "Social network"
                    |> withProperties
                        [ ( "friends"
                          , buildSchema
                                |> withTitle "Add a friend"
                                |> withDescription "You can add at most five friends."
                                |> withType "array"
                                |> withUniqueItems True
                                |> withMaxItems 5
                                |> withItem
                                    (buildSchema
                                        |> withType "object"
                                        |> withProperties
                                            [ ( "name"
                                              , buildSchema
                                                    |> withType "string"
                                                    |> withTitle "Name"
                                              )
                                            , ( "email"
                                              , buildSchema
                                                    |> withType "string"
                                                    |> withTitle "Email"
                                                    |> withFormat "email"
                                              )
                                            ]
                                    )
                          )
                        ]
              )
            , ( "contact"
              , buildSchema
                    |> withType "object"
                    |> withTitle "Contact details"
                    |> withDescription "Please enter your contact details."
                    |> withProperties
                        [ ( "email"
                          , buildSchema
                                |> withType "string"
                                |> withTitle "Email"
                                |> withDescription "Your email address."
                                |> withFormat "email"
                                |> withDefault (string "a@example.com")
                          )
                        , ( "phone"
                          , buildSchema
                                |> withType "string"
                                |> withTitle "Phone"
                                |> withDescription "Your phone number."
                                |> withFormat "phone"
                          )
                        ]
              )
            , ( "terms"
              , buildSchema
                    |> withType "boolean"
                    |> withTitle "Jag accepterar villkoren"
                    |> withConst (bool True)
              )
            ]
        |> toSchema


init : Schema.State
init =
    case schema of
        Ok s ->
            Schema.init s

        Err error ->
            Debug.todo error


update : Schema.Msg -> Schema.State -> Schema.State
update msg state =
    Schema.update msg state


view : Schema.State -> Html Schema.Msg
view state =
    form [ onSubmit Form.Submit ]
        [ Schema.view state
        , button [ class "btn btn-primary" ] [ text "Submit" ]
        , case Form.getOutput state.form of
            Just output ->
                let
                    json =
                        Json.Encode.encode 4 (Schema.Encode.encode output)
                in
                pre [] [ text json ]

            Nothing ->
                text ""
        ]
