module Schema.Error exposing
    ( ValidationError(..)
    , errorString
    )

import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))


type ValidationError
    = Invalid
    | InvalidSet
    | ShorterListThan Int
    | LongerListThan Int


errorString : ErrorValue ValidationError -> String
errorString error =
    case error of
        Empty ->
            "Fältet får inte vara tomt."

        InvalidString ->
            "Du måste fylla i fältet."

        InvalidEmail ->
            "Det är inte en giltig e-postadress."

        InvalidFormat ->
            "Fältet har inte rätt format."

        InvalidInt ->
            "Det är inte en siffra."

        InvalidFloat ->
            "Det är inte ett flyttal."

        InvalidBool ->
            "Du måste svara ja eller nej."

        SmallerIntThan n ->
            "Får inte vara mindre än " ++ String.fromInt n ++ "."

        GreaterIntThan n ->
            "Får inte vara större än " ++ String.fromInt n ++ "."

        SmallerFloatThan n ->
            "Får inte vara mindre än " ++ String.fromFloat n ++ "."

        GreaterFloatThan n ->
            "Får inte vara större än " ++ String.fromFloat n ++ "."

        ShorterStringThan n ->
            "Måste vara minst " ++ String.fromInt n ++ " tecken."

        LongerStringThan n ->
            "Får inte vara längre än " ++ String.fromInt n ++ " tecken."

        NotIncludedIn ->
            "Är inte ett korrekt val från listan."

        CustomError Invalid ->
            "Invalid"

        CustomError InvalidSet ->
            "Den kombination av val du har gjort är inte tillåten."

        CustomError (ShorterListThan n) ->
            "Måste innehålla minst " ++ String.fromInt n ++ " saker."

        CustomError (LongerListThan n) ->
            "Får inte innehålla mer än " ++ String.fromInt n ++ " saker."
