module Schema.Form exposing (Form, schemaView)

import Form as F
import Form.Error exposing (ErrorValue(..))
import Form.Input as Input
import Form.Validate exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import Json.Schema
import Json.Schema.Definitions
    exposing
        ( Schema(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        )
import Schema.Error exposing (ValidationError, errorString)
import Schema.Validation exposing (validation)
import Schema.Value exposing (Value(..))


type alias Form =
    F.Form ValidationError Value


type alias Path =
    List String


type alias ErrorString e =
    ErrorValue e -> String


schemaView : Path -> Schema -> Form -> Html F.Msg
schemaView path schema form =
    case schema of
        BooleanSchema value ->
            div []
                [ if value == True then
                    text "True"

                  else
                    text "False"
                ]

        ObjectSchema subSchema ->
            objectView path subSchema form


objectView : Path -> SubSchema -> Form -> Html F.Msg
objectView path schema form =
    case schema.type_ of
        AnyType ->
            fieldset [] []

        NullableType singleType ->
            fieldView path schema singleType form

        UnionType _ ->
            fieldset [] []

        SingleType singleType ->
            fieldView path schema singleType form


fieldView : Path -> SubSchema -> SingleType -> Form -> Html F.Msg
fieldView path schema type_ form =
    case type_ of
        IntegerType ->
            txt schema (getFieldAsString path form)

        NumberType ->
            txt schema (getFieldAsString path form)

        StringType ->
            case schema.oneOf of
                Just _ ->
                    select schema (getFieldAsString path form)

                Nothing ->
                    txt schema (getFieldAsString path form)

        BooleanType ->
            checkbox schema (getFieldAsBool path form)

        ArrayType ->
            fieldset []
                []

        ObjectType ->
            let
                f =
                    getFieldAsString path form

                schemataItem ( name, subSchema ) =
                    schemaView (path ++ [ name ]) subSchema form

                fields =
                    case schema.properties of
                        Nothing ->
                            []

                        Just (Json.Schema.Definitions.Schemata schemata) ->
                            List.map schemataItem schemata

                meta =
                    [ Maybe.map (\str -> h3 [] [ text str ]) schema.title
                    , Maybe.map (\str -> p [] [ text str ]) schema.description
                    , Maybe.map (error errorString) f.liveError
                    ]
                        |> List.filterMap identity
            in
            div [ id (fieldPath path), tabindex -1 ] (meta ++ fields)

        NullType ->
            fieldset []
                []


txt : SubSchema -> F.FieldState ValidationError String -> Html F.Msg
txt schema f =
    field schema f <|
        Input.textInput f
            [ class "form-control"
            , id f.path
            ]


checkbox : SubSchema -> F.FieldState ValidationError Bool -> Html F.Msg
checkbox schema f =
    fieldset
        [ classList
            [ ( "form-group", True )
            , ( "form-check", True )
            , ( "is-invalid", f.liveError /= Nothing )
            ]
        ]
        [ label [ class "form-check-label" ]
            [ Input.checkboxInput f [ class "form-check-input", id f.path ]
            , text (schema.title |> Maybe.withDefault "")
            , case schema.description of
                Just str ->
                    div [] [ small [] [ text str ] ]

                Nothing ->
                    text ""
            ]
        , case f.liveError of
            Just err ->
                error errorString err

            Nothing ->
                text ""
        ]


select : SubSchema -> F.FieldState ValidationError String -> Html F.Msg
select schema f =
    let
        options =
            case schema.oneOf of
                Just values ->
                    List.map option values

                Nothing ->
                    []
    in
    field schema f <|
        Input.selectInput ([ ( "", "" ) ] ++ options)
            f
            [ class "form-control custom-select"
            , id f.path
            ]


option : Schema -> ( String, String )
option schema =
    case schema of
        BooleanSchema _ ->
            ( "", "" )

        ObjectSchema schema_ ->
            ( schema_.const
                |> Maybe.map (Json.Decode.decodeValue Json.Decode.string)
                |> Maybe.map (Result.withDefault "")
                |> Maybe.withDefault ""
            , schema_.title |> Maybe.withDefault ""
            )


field : SubSchema -> F.FieldState ValidationError String -> Html F.Msg -> Html F.Msg
field schema f content =
    fieldset
        [ classList
            [ ( "form-group", True )
            , ( "is-invalid", f.liveError /= Nothing )
            , ( "has-value", f.value /= Nothing && f.value /= Just "" )
            ]
        ]
        [ label [ for f.path ]
            [ case schema.title of
                Just str ->
                    span [ class "label-text" ] [ text str ]

                Nothing ->
                    text ""
            , content
            , case schema.description of
                Just str ->
                    div [] [ small [] [ text str ] ]

                Nothing ->
                    text ""
            ]
        , case f.liveError of
            Just err ->
                error errorString err

            Nothing ->
                text ""
        ]


error : ErrorString ValidationError -> ErrorValue ValidationError -> Html F.Msg
error func err =
    div [ class "invalid-feedback" ] [ text (func err) ]


getFieldAsBool : Path -> F.Form e o -> F.FieldState e Bool
getFieldAsBool path =
    F.getFieldAsBool (fieldPath path)


getFieldAsString : Path -> F.Form e o -> F.FieldState e String
getFieldAsString path =
    F.getFieldAsString (fieldPath path)


fieldPath : Path -> String
fieldPath =
    String.join "."
