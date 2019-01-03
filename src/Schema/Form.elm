module Schema.Form exposing (Form, schemaView)

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
            fieldView path schema BooleanType form

        NullableType singleType ->
            fieldView path schema singleType form

        UnionType _ ->
            fieldView path schema StringType form

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
            if schema.oneOf /= Nothing || schema.anyOf /= Nothing then
                select schema (getFieldAsString path form)

            else
                txt schema (getFieldAsString path form)

        BooleanType ->
            checkbox schema (getFieldAsBool path form)

        ArrayType ->
            let
                f =
                    getFieldAsString path form
            in
            case schema.items of
                NoItems ->
                    field schema f (list path form ( schema.title, blankSchema ))

                ItemDefinition item ->
                    field schema f (list path form ( schema.title, item ))

                ArrayOfItems items ->
                    field schema f (tuple path form items)

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
            in
            group schema f fields

        NullType ->
            fieldset []
                []


txt : SubSchema -> F.FieldState ValidationError String -> Html F.Msg
txt schema f =
    field schema
        f
        [ fieldTitle schema
        , Input.textInput f
            [ classList
                [ ( "form-control", True )
                , ( "is-invalid", f.liveError /= Nothing )
                ]
            , id f.path
            ]
        ]


checkbox : SubSchema -> F.FieldState ValidationError Bool -> Html F.Msg
checkbox schema f =
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
            , case f.liveError of
                Just err ->
                    error errorString err

                Nothing ->
                    text ""
            , case schema.description of
                Just str ->
                    fieldDescription str

                Nothing ->
                    text ""
            ]
        ]


select : SubSchema -> F.FieldState ValidationError String -> Html F.Msg
select schema f =
    let
        schemata =
            List.concat
                [ schema.oneOf |> Maybe.withDefault []
                , schema.anyOf |> Maybe.withDefault []
                ]

        options =
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
    field schema
        f
        [ fieldTitle schema
        , Input.selectInput
            options
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
    Path
    -> Form
    -> List Schema
    -> List (Html F.Msg)
set path form items =
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
            schemaView (itemPath schema_) schema_ form
    in
    List.map itemView items


list :
    Path
    -> Form
    -> ( Maybe String, Schema )
    -> List (Html F.Msg)
list path form ( title, schema ) =
    let
        indexes =
            getListIndexes path form

        itemPath idx =
            path ++ [ String.fromInt idx ]

        itemView idx =
            li
                [ class "list-group-item" ]
                [ schemaView (itemPath idx) schema form
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
    Path
    -> Form
    -> List Schema
    -> List (Html F.Msg)
tuple path form schemata =
    let
        itemPath idx =
            path ++ [ "tuple" ++ String.fromInt idx ]

        itemView idx schema =
            div
                [ class "col" ]
                [ schemaView (itemPath idx) schema form ]
    in
    [ div [ class "form-row" ] (List.indexedMap itemView schemata) ]


field : SubSchema -> F.FieldState ValidationError String -> List (Html F.Msg) -> Html F.Msg
field schema f content =
    let
        meta =
            [ Maybe.map fieldDescription schema.description ]
                |> List.filterMap identity

        feedback =
            [ Maybe.map (error errorString) f.liveError ]
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


group : SubSchema -> F.FieldState ValidationError String -> List (Html F.Msg) -> Html F.Msg
group schema f content =
    let
        meta =
            [ Maybe.map (\str -> legend [] [ text str ]) schema.title
            , Maybe.map (\str -> p [] [ text str ]) schema.description
            ]
                |> List.filterMap identity

        feedback =
            [ Maybe.map (error errorString) f.liveError
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


error : ErrorString ValidationError -> ErrorValue ValidationError -> Html F.Msg
error func err =
    div
        [ class "invalid-feedback"
        , style "display" "block"
        ]
        [ text (func err) ]


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
