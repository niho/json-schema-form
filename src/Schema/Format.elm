module Schema.Format exposing
    ( date
    , dateTime
    , email
    , hostname
    , ipv4
    , ipv6
    , time
    )

import Regex



{-
   date-time
   date
   time
   email
   idn-email
   hostname
   idn-hostname
   ipv4
   ipv6
   uri
   uri-reference
   iri
   iri-reference
   uri-template
   json-pointer
   relative-json-pointer
-}


dateTime : Regex.Regex
dateTime =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^\\d\\d\\d\\d-[0-1]\\d-[0-3]\\d[t\\s](?:[0-2]\\d:[0-5]\\d:[0-5]\\d|23:59:60)(?:\\.\\d+)?(?:z|[+-]\\d\\d:\\d\\d)$"


date : Regex.Regex
date =
    Maybe.withDefault Regex.never <|
        Regex.fromString
            "^\\d\\d\\d\\d-[0-1]\\d-[0-3]\\d$"


time : Regex.Regex
time =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^(?:[0-2]\\d:[0-5]\\d:[0-5]\\d|23:59:60)(?:\\.\\d+)?(?:z|[+-]\\d\\d:\\d\\d)?$"


email : Regex.Regex
email =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\\.[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)*$"


hostname : Regex.Regex
hostname =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\\.[a-z0-9](?:[-0-9a-z]{0,61}[0-9a-z])?)*$"


ipv4 : Regex.Regex
ipv4 =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^(?:(?:25[0-5]|2[0-4]\\d|[01]?\\d\\d?)\\.){3}(?:25[0-5]|2[0-4]\\d|[01]?\\d\\d?)$"


ipv6 : Regex.Regex
ipv6 =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^\\s*(?:(?:(?:[0-9a-f]{1,4}:){7}(?:[0-9a-f]{1,4}|:))|(?:(?:[0-9a-f]{1,4}:){6}(?::[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|(?:(?:[0-9a-f]{1,4}:){5}(?:(?:(?::[0-9a-f]{1,4}){1,2})|:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|(?:(?:[0-9a-f]{1,4}:){4}(?:(?:(?::[0-9a-f]{1,4}){1,3})|(?:(?::[0-9a-f]{1,4})?:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){3}(?:(?:(?::[0-9a-f]{1,4}){1,4})|(?:(?::[0-9a-f]{1,4}){0,2}:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){2}(?:(?:(?::[0-9a-f]{1,4}){1,5})|(?:(?::[0-9a-f]{1,4}){0,3}:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){1}(?:(?:(?::[0-9a-f]{1,4}){1,6})|(?:(?::[0-9a-f]{1,4}){0,4}:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|(?::(?:(?:(?::[0-9a-f]{1,4}){1,7})|(?:(?::[0-9a-f]{1,4}){0,5}:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:)))(?:%.+)?\\s*$"
