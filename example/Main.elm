module Main exposing (main)

import Browser
import Form exposing (Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Validate
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Json.Encode exposing (bool, float, int, list, string)
import Json.Schema.Builder exposing (..)
import Json.Schema.Definitions
import Regex
import Schema
import Schema.Encode
import Schema.Error exposing (ValidationError(..))
import Schema.Validation exposing (Formats)


main : Program () Schema.State Schema.Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : Schema.State
init =
    case schema of
        Ok schema_ ->
            Schema.init { errors = errorString, formats = formats } schema_

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


errorString : Schema.Errors
errorString path error =
    case error of
        Empty ->
            "The field can not be empty."

        InvalidString ->
            "This field is required."

        InvalidEmail ->
            "That is not a valid email address."

        InvalidFormat ->
            "That is not the correct format."

        InvalidInt ->
            "That is not a valid number."

        InvalidFloat ->
            "That is not a valid decimal number."

        InvalidBool ->
            "That is not a valid option."

        SmallerIntThan n ->
            "Can not be smaller than " ++ String.fromInt n ++ "."

        GreaterIntThan n ->
            "Can not be greater than " ++ String.fromInt n ++ "."

        SmallerFloatThan n ->
            "Can not be smaller than " ++ String.fromFloat n ++ "."

        GreaterFloatThan n ->
            "Can not be greater than " ++ String.fromFloat n ++ "."

        ShorterStringThan n ->
            "Must be at least " ++ String.fromInt n ++ " characters long."

        LongerStringThan n ->
            "Can not be more than " ++ String.fromInt n ++ " characters long."

        NotIncludedIn ->
            "Is not a valid selection from the list."

        CustomError Invalid ->
            "Is not valid."

        CustomError InvalidSet ->
            "All items added need to be unique."

        CustomError (ShorterListThan n) ->
            if path == "airports" then
                "You need to add at least " ++ String.fromInt n ++ " airports."

            else
                "You need to add at least " ++ String.fromInt n ++ " items."

        CustomError (LongerListThan n) ->
            "You can not add more than " ++ String.fromInt n ++ " items."

        CustomError (InvalidCustomFormat format) ->
            case format of
                "personal-number" ->
                    "That is not a valid personal number."

                _ ->
                    "That is not the correct format."


personalNumber : Regex.Regex
personalNumber =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^(19|20)[0-9]{6}[0-9]{4}$"


formats : Formats
formats =
    [ ( "personal-number"
      , { title = Nothing
        , validation = Form.Validate.format personalNumber
        }
      )
    ]


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
            , ( "numbers"
              , buildSchema
                    |> withType "array"
                    |> withDefault (list float [ 6, 6.8 ])
                    |> withItems
                        [ buildSchema
                            |> withType "integer"
                            |> withTitle "Integer"
                            |> withDescription "Enter an integer between 5-10."
                            |> withExclusiveMinimum 5
                            |> withExclusiveMaximum 10
                            |> withMultipleOf 2
                        , buildSchema
                            |> withType "number"
                            |> withTitle "Number"
                            |> withDescription "Enter a natural number between 5.5-10.1 (inclusive)."
                            |> withMinimum 5.5
                            |> withMaximum 10.1
                        ]
              )
            , ( "color"
              , buildSchema
                    |> withTitle "Color"
                    |> withExamples
                        [ string "Please enter either red, gren or blue."
                        ]
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
                                [ boolSchema False
                                , buildSchema
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
                        , ( "personal_number"
                          , buildSchema
                                |> withType "string"
                                |> withTitle "Personal number"
                                |> withDescription "Your personal number."
                                |> withFormat "personal-number"
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
