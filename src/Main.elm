module Main exposing (main)

import Browser
import Form exposing (Msg(..))
import Html exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (bool, string)
import Json.Schema.Builder exposing (..)
import Schema
import Schema.Encode


main =
    Browser.sandbox { init = init, update = update, view = view }


init =
    let
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
                    , ( "foo"
                      , buildSchema
                            |> withType "integer"
                            |> withExclusiveMinimum 5
                            |> withExclusiveMaximum 10
                      )
                    , ( "bar"
                      , buildSchema
                            |> withType "number"
                            |> withMinimum 5.5
                            |> withMaximum 10.1
                      )
                    , ( "color"
                      , buildSchema
                            |> withTitle "Select a color"
                            |> withDescription "Choose any of three colors."
                            |> withType "string"
                            |> withEnum
                                [ string "red"
                                , string "green"
                                , string "blue"
                                ]
                      )
                    , ( "contact"
                      , buildSchema
                            |> withType "object"
                            |> withProperties
                                [ ( "email"
                                  , buildSchema
                                        |> withType "string"
                                        |> withTitle "Email"
                                        |> withDescription "Your email address."
                                        |> withFormat "email"
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
    in
    case schema of
        Ok s ->
            Schema.init s

        Err error ->
            Debug.todo error


update msg state =
    Debug.log "state" (Schema.update msg state)


view state =
    form [ onSubmit Form.Submit ]
        [ Schema.view state
        , button [] [ text "Submit" ]
        , case Form.getOutput state.form of
            Just output ->
                pre []
                    [ text (Json.Encode.encode 4 (Schema.Encode.encode output)) ]

            Nothing ->
                text ""
        ]
