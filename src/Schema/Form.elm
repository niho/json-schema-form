module Schema.Form exposing (Form, Options, schemaView)

import Dict
import Form as F
import Form.Error exposing (ErrorValue(..))
import Form.Input as Input
import Form.Validate exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode
import Json.Schema
import Json.Schema.Definitions
    exposing
        ( Items(..)
        , Schema(..)
        , SingleType(..)
        , SubSchema
        , Type(..)
        , blankSchema
        )
import Schema.Error exposing (Errors, ValidationError)
import Schema.Validation exposing (Formats, getFormat, validation)
import Schema.Value exposing (Value(..))


type alias Options =
    { errors : Errors ValidationError
    , formats : Formats
    }


type alias Form =
    F.Form ValidationError Value


type alias Path =
    List String


schemaView : Options -> Path -> Schema -> Form -> Html F.Msg
schemaView options path schema form =
    case schema of
        BooleanSchema value ->
            div []
                [ if value == True then
                    text "True"

                  else
                    text "False"
                ]

        ObjectSchema subSchema ->
            objectView options path subSchema form


objectView : Options -> Path -> SubSchema -> Form -> Html F.Msg
objectView options path schema form =
    case schema.type_ of
        AnyType ->
            fieldView options path schema BooleanType form

        NullableType singleType ->
            fieldView options path schema singleType form

        UnionType _ ->
            fieldView options path schema StringType form

        SingleType singleType ->
            fieldView options path schema singleType form


fieldView : Options -> Path -> SubSchema -> SingleType -> Form -> Html F.Msg
fieldView options path schema type_ form =
    case type_ of
        IntegerType ->
            txt options schema (getFieldAsString path form)

        NumberType ->
            txt options schema (getFieldAsString path form)

        StringType ->
            if schema.oneOf /= Nothing || schema.anyOf /= Nothing then
                select options schema (getFieldAsString path form)

            else
                txt options schema (getFieldAsString path form)

        BooleanType ->
            checkbox options schema (getFieldAsBool path form)

        ArrayType ->
            let
                f =
                    getFieldAsString path form
            in
            case schema.items of
                NoItems ->
                    field options schema f (list options path form ( schema.title, blankSchema ))

                ItemDefinition item ->
                    field options schema f (list options path form ( schema.title, item ))

                ArrayOfItems items ->
                    field options schema f (tuple options path form items)

        ObjectType ->
            let
                f =
                    getFieldAsString path form

                schemataItem ( name, subSchema ) =
                    schemaView options (path ++ [ name ]) subSchema form

                fields =
                    case schema.properties of
                        Nothing ->
                            []

                        Just (Json.Schema.Definitions.Schemata schemata) ->
                            List.map schemataItem schemata
            in
            group options schema f fields

        NullType ->
            fieldset []
                []


txt : Options -> SubSchema -> F.FieldState ValidationError String -> Html F.Msg
txt options schema f =
    let
        attributes =
            [ classList
                [ ( "form-control", True )
                , ( "is-invalid", f.liveError /= Nothing )
                ]
            , id f.path
            ]

        placeholders =
            case schema.examples of
                Just examples ->
                    examples
                        |> List.map (Json.Decode.decodeValue Json.Decode.string)
                        |> List.map Result.toMaybe
                        |> List.filterMap identity
                        |> List.map placeholder

                Nothing ->
                    []
    in
    field options
        schema
        f
        [ fieldTitle schema
        , case schema.format of
            Just "email" ->
                Input.textInput f
                    (attributes ++ placeholders ++ [ type_ "email" ])

            Just format ->
                case getFormat format options.formats |> Maybe.andThen .title of
                    Just title ->
                        inputGroup title
                            [ Input.textInput f (attributes ++ placeholders) ]

                    _ ->
                        Input.textInput f (attributes ++ placeholders)

            Nothing ->
                Input.textInput f (attributes ++ placeholders)
        ]


checkbox : Options -> SubSchema -> F.FieldState ValidationError Bool -> Html F.Msg
checkbox options schema f =
    div
        [ classList
            [ ( "form-group", True )
            , ( "form-check", True )
            , ( "is-invalid", f.liveError /= Nothing )
            ]
        ]
        [ label [ class "form-check-label" ]
            [ Input.checkboxInput f
                [ classList
                    [ ( "form-check-input", True )
                    , ( "is-invalid", f.liveError /= Nothing )
                    ]
                , id f.path
                ]
            , text (schema.title |> Maybe.withDefault "")
            , liveError options.errors f |> Maybe.withDefault (text "")
            , case schema.description of
                Just str ->
                    fieldDescription str

                Nothing ->
                    text ""
            ]
        ]


select : Options -> SubSchema -> F.FieldState ValidationError String -> Html F.Msg
select options schema f =
    let
        schemata =
            List.concat
                [ schema.oneOf |> Maybe.withDefault []
                , schema.anyOf |> Maybe.withDefault []
                ]

        items =
            schemata
                |> List.map option
                |> List.map
                    (\( name, schema_ ) ->
                        ( name
                        , schema_
                            |> Maybe.andThen .title
                            |> Maybe.withDefault name
                        )
                    )

        descriptions =
            schemata
                |> List.map option
                |> List.map
                    (\( name, schema_ ) ->
                        ( name
                        , schema_
                            |> Maybe.andThen .description
                            |> Maybe.map fieldDescription
                            |> Maybe.withDefault (text "")
                        )
                    )
    in
    field options
        schema
        f
        [ fieldTitle schema
        , Input.selectInput
            items
            f
            [ classList
                [ ( "form-control custom-select", True )
                , ( "is-invalid", f.liveError /= Nothing )
                ]
            , id f.path
            ]
        , conditional f descriptions
        ]


option : Schema -> ( String, Maybe SubSchema )
option schema =
    case schema of
        BooleanSchema _ ->
            ( "", Nothing )

        ObjectSchema schema_ ->
            ( constAsString schema_ |> Maybe.withDefault ""
            , Just schema_
            )


set :
    Options
    -> Path
    -> Form
    -> List Schema
    -> List (Html F.Msg)
set options path form items =
    let
        itemPath schema_ =
            case schema_ of
                ObjectSchema s ->
                    path
                        ++ [ constAsString s
                                |> Maybe.withDefault ""
                           ]

                _ ->
                    path

        itemView schema_ =
            schemaView options (itemPath schema_) schema_ form
    in
    List.map itemView items


list :
    Options
    -> Path
    -> Form
    -> ( Maybe String, Schema )
    -> List (Html F.Msg)
list options path form ( title, schema ) =
    let
        indexes =
            getListIndexes path form

        itemPath idx =
            path ++ [ String.fromInt idx ]

        itemView idx =
            li
                [ class "list-group-item bg-light" ]
                [ schemaView options (itemPath idx) schema form
                , button
                    [ onClickPreventDefault (F.RemoveItem (fieldPath path) idx)
                    , class "btn btn-outline-secondary btn-sm btn-remove"
                    ]
                    [ text "Remove" ]
                ]
    in
    [ ol [ class "list-group mb-2" ] (List.map itemView indexes)
    , button
        [ class "btn btn-secondary btn-add"
        , onClickPreventDefault (F.Append (fieldPath path))
        ]
        [ text (title |> Maybe.withDefault "Add")
        ]
    ]


tuple :
    Options
    -> Path
    -> Form
    -> List Schema
    -> List (Html F.Msg)
tuple options path form schemata =
    let
        itemPath idx =
            path ++ [ "tuple" ++ String.fromInt idx ]

        itemView idx schema =
            div
                [ class "col" ]
                [ schemaView options (itemPath idx) schema form ]
    in
    [ div [ class "form-row" ] (List.indexedMap itemView schemata) ]


field : Options -> SubSchema -> F.FieldState ValidationError String -> List (Html F.Msg) -> Html F.Msg
field options schema f content =
    let
        meta =
            [ Maybe.map fieldDescription schema.description ]
                |> List.filterMap identity

        feedback =
            [ liveError options.errors f ]
                |> List.filterMap identity
    in
    div
        [ classList
            [ ( "form-group", True )
            , ( "is-invalid", f.liveError /= Nothing )
            , ( "has-value", f.value /= Nothing && f.value /= Just "" )
            ]
        ]
        [ label [ for f.path, class "d-block" ]
            (content ++ feedback ++ meta)
        ]


group : Options -> SubSchema -> F.FieldState ValidationError String -> List (Html F.Msg) -> Html F.Msg
group options schema f content =
    let
        meta =
            [ Maybe.map (\str -> legend [] [ text str ]) schema.title
            , Maybe.map (\str -> p [] [ text str ]) schema.description
            ]
                |> List.filterMap identity

        feedback =
            [ liveError options.errors f
            ]
                |> List.filterMap identity
    in
    fieldset
        [ name f.path
        , id f.path
        , tabindex -1
        ]
        (meta ++ content ++ feedback)


fieldTitle : SubSchema -> Html F.Msg
fieldTitle schema =
    case schema.title of
        Just str ->
            span [ class "label-text" ] [ text str ]

        Nothing ->
            text ""


fieldDescription : String -> Html F.Msg
fieldDescription str =
    div [ class "form-text text-muted" ] [ text str ]


liveError : Errors ValidationError -> F.FieldState ValidationError a -> Maybe (Html F.Msg)
liveError func f =
    f.liveError
        |> Maybe.map
            (\err ->
                div
                    [ class "invalid-feedback"
                    , style "display" "block"
                    ]
                    [ text (func f.path err) ]
            )


inputGroup : String -> List (Html F.Msg) -> Html F.Msg
inputGroup title content =
    div
        [ class "input-group" ]
        ([ div
            [ class "input-group-prepend" ]
            [ div
                [ class "input-group-text" ]
                [ text title ]
            ]
         ]
            ++ content
        )


getFieldAsBool : Path -> F.Form e o -> F.FieldState e Bool
getFieldAsBool path =
    F.getFieldAsBool (fieldPath path)


getFieldAsString : Path -> F.Form e o -> F.FieldState e String
getFieldAsString path =
    F.getFieldAsString (fieldPath path)


getListIndexes : Path -> F.Form e o -> List Int
getListIndexes path =
    F.getListIndexes (fieldPath path)


fieldPath : Path -> String
fieldPath =
    String.join "."


constAsString : SubSchema -> Maybe String
constAsString schema =
    schema.const
        |> Maybe.map (Json.Decode.decodeValue Json.Decode.string)
        |> Maybe.andThen Result.toMaybe


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    preventDefaultOn "click"
        (Json.Decode.map alwaysPreventDefault
            (Json.Decode.succeed msg)
        )


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


conditional : F.FieldState e String -> List ( String, Html F.Msg ) -> Html F.Msg
conditional f conditions =
    let
        cond ( value, html ) =
            if f.value == Just value then
                Just ( value, html )

            else
                Nothing
    in
    Html.Keyed.node "div" [] <| List.filterMap cond conditions
