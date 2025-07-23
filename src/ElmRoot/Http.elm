module ElmRoot.Http exposing (AccessControlAllowOrigin(..), AuthorizationType(..), CacheDirective(..), ConnectionType(..), ContentDisposition(..), Encoding(..), HttpMethod(..), MimeType(..), RequestHeader(..), ResponseHeader(..), RetryAfter(..), StrictTransportSecurity(..), XFrameOptions(..), authorizationDecoder, cacheDirectiveDecoder, connectionTypeDecoder, decodeRequestHeader, encodeResponseHeader, encodingDecoder, formatImfFixdate, httpMethodDecoder, httpMethodEncoder, imfFixdateDecoder, imfFixdateEncoder, mimeTypeDecoder, parseImfFixdate)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Time


type HttpMethod
    = GET
    | POST
    | PUT
    | PATCH
    | DELETE
    | HEAD
    | OPTIONS


type MimeType
    = ApplicationFormUrlEncoded
    | ApplicationJson
    | ApplicationOctetStream
    | ApplicationPdf
    | ApplicationZip
    | AudioMp3
    | AudioWebm
    | CustomMime String
    | ImageAvif
    | ImageGif
    | ImageJpeg
    | ImagePng
    | ImageSvg
    | ImageWebp
    | MultipartFormData
    | TextCss
    | TextHtml
    | TextJavascript
    | TextPlain
    | VideoMp4
    | VideoWebm


type Encoding
    = Br
    | Deflate
    | Gzip
    | Identity
    | Star


type CacheDirective
    = MaxAge Int
    | SMaxAge Int
    | MustRevalidate
    | ProxyRevalidate
    | NoCache
    | NoStore
    | Private
    | Public
    | MustUnderstand
    | NoTransform
    | Immutable
    | StaleWhileRevalidate Int
    | StaleIfError Int


type ConnectionType
    = Close
    | KeepAlive
    | Upgrade


type AuthorizationType
    = Basic String
    | Bearer String
    | CustomAuth String String


type AccessControlAllowOrigin
    = Wildcard


type ContentDisposition
    = Inline (Maybe String)
    | Attachment (Maybe String)
    | FormData String (Maybe String)


type XFrameOptions
    = Deny
    | SameOrigin


type RetryAfter
    = Seconds Int
    | HttpDate Time.Posix


type StrictTransportSecurity
    = StrictTransportSecurity { maxAge : Int, includeSubDomains : Bool, preload : Bool }



-- Implemented based on:
--   https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/MIME_types
--   https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Content_negotiation/List_of_default_Accept_values


type RequestHeader
    = RequestAccept (List { mimeType : MimeType, preference : Float })
    | RequestAcceptEncoding (List { encoding : Encoding, preference : Float })
    | RequestAcceptLanguage (List { language : String, preference : Float })
    | RequestAuthorization AuthorizationType
    | RequestCacheControl (List CacheDirective)
    | RequestConnection ConnectionType
    | RequestContentEncoding Encoding
    | RequestContentLanguage String
    | RequestContentLength Int
    | RequestContentType MimeType
    | RequestCookie (List { name : String, value : String })
    | RequestCustomHeader String String
    | RequestHost String
    | RequestIfMatch String
    | RequestIfModifiedSince String
    | RequestIfNoneMatch String
    | RequestIfRange String
    | RequestIfUnmodifiedSince String
    | RequestOrigin String
    | RequestReferrer String
    | RequestUserAgent String



-- https://flaviocopes.com/http-response-headers/


type ResponseHeader
    = ResponseAccessControlAllowOrigin AccessControlAllowOrigin
    | ResponseAccessControlAllowCredentials Bool
    | ResponseAccessControlExposeHeaders (List String)
    | ResponseAccessControlMaxAge Int
    | ResponseAccessControlAllowMethods (List HttpMethod)
    | ResponseAccessControlAllowHeaders (List String)
    | ResponseAllow (List HttpMethod)
    | ResponseCacheControl (List CacheDirective)
    | ResponseContentDisposition ContentDisposition
    | ResponseContentEncoding Encoding
    | ResponseContentLanguage String
    | ResponseContentLength Int
    | ResponseContentType MimeType
    | ResponseCustomHeader String String
    | ResponseETag String
    | ResponseExpires Time.Posix
    | ResponseLastModified Time.Posix
    | ResponseLocation String
    | ResponseRetryAfter RetryAfter
    | ResponseServer String
    | ResponseSetCookie (List { name : String, value : String })
    | ResponseStrictTransportSecurity StrictTransportSecurity
    | ResponseXFrameOptions XFrameOptions


encodeResponseHeader : ResponseHeader -> Encode.Value
encodeResponseHeader header =
    let
        ( key, value ) =
            case header of
                ResponseAccessControlAllowOrigin origin ->
                    ( "Access-Control-Allow-Origin", accessControlAllowOriginToString origin )

                ResponseAccessControlAllowCredentials bool ->
                    ( "Access-Control-Allow-Credentials"
                    , if bool then
                        "true"

                      else
                        "false"
                    )

                ResponseAccessControlExposeHeaders headers ->
                    ( "Access-Control-Expose-Headers", String.join ", " headers )

                ResponseAccessControlMaxAge seconds ->
                    ( "Access-Control-Max-Age", String.fromInt seconds )

                ResponseAccessControlAllowMethods methods ->
                    ( "Access-Control-Allow-Methods", httpMethodListToString methods )

                ResponseAccessControlAllowHeaders headers ->
                    ( "Access-Control-Allow-Headers", String.join ", " headers )

                ResponseAllow methods ->
                    ( "Allow", httpMethodListToString methods )

                ResponseCacheControl directives ->
                    ( "Cache-Control", cacheDirectiveListToString directives )

                ResponseContentDisposition disposition ->
                    ( "Content-Disposition", contentDispositionToString disposition )

                ResponseContentEncoding encoding ->
                    ( "Content-Encoding", encodingToString encoding )

                ResponseContentLanguage language ->
                    ( "Content-Language", language )

                ResponseContentLength length ->
                    ( "Content-Length", String.fromInt length )

                ResponseContentType mimeType ->
                    ( "Content-Type", mimeTypeToString mimeType )

                ResponseCustomHeader customKey customValue ->
                    ( customKey, customValue )

                ResponseETag etag ->
                    ( "ETag", etag )

                ResponseExpires posix ->
                    ( "Expires", formatImfFixdate posix )

                ResponseLastModified posix ->
                    ( "Last-Modified", formatImfFixdate posix )

                ResponseLocation location ->
                    ( "Location", location )

                ResponseRetryAfter retry ->
                    ( "Retry-After", retryAfterToString retry )

                ResponseServer server ->
                    ( "Server", server )

                ResponseSetCookie cookies ->
                    ( "Set-Cookie", String.join "; " (List.map cookieToString cookies) )

                ResponseStrictTransportSecurity hsts ->
                    ( "Strict-Transport-Security", strictTransportSecurityToString hsts )

                ResponseXFrameOptions option ->
                    ( "X-Frame-Options", xFrameOptionsToString option )
    in
    Encode.object
        [ ( "key", Encode.string key )
        , ( "value", Encode.string value )
        ]


decodeRequestHeader : Decode.Decoder RequestHeader
decodeRequestHeader =
    Decode.succeed (\name value -> ( String.toLower name, value ))
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "value" Decode.string
        |> Decode.andThen
            (\( lowercaseName, value ) ->
                case lowercaseName of
                    "accept" ->
                        parseCommaSeparatedQualityList mimeTypeDecoder (\mimeType pref -> { mimeType = mimeType, preference = pref }) value
                            |> Decode.map RequestAccept

                    "accept-encoding" ->
                        parseCommaSeparatedQualityList encodingDecoder (\encoding pref -> { encoding = encoding, preference = pref }) value
                            |> Decode.map RequestAcceptEncoding

                    "accept-language" ->
                        parseCommaSeparatedQualityList Decode.string (\language pref -> { language = language, preference = pref }) value
                            |> Decode.map RequestAcceptLanguage

                    "authorization" ->
                        case Decode.decodeString authorizationDecoder ("\"" ++ value ++ "\"") of
                            Ok auth ->
                                Decode.succeed (RequestAuthorization auth)

                            Err _ ->
                                Decode.fail ("Invalid authorization: " ++ value)

                    "cache-control" ->
                        let
                            directives =
                                String.split "," value
                                    |> List.map String.trim
                                    |> List.filterMap
                                        (\directive ->
                                            case Decode.decodeString cacheDirectiveDecoder ("\"" ++ directive ++ "\"") of
                                                Ok parsed ->
                                                    Just parsed

                                                Err _ ->
                                                    Nothing
                                        )
                        in
                        Decode.succeed (RequestCacheControl directives)

                    "connection" ->
                        case Decode.decodeString connectionTypeDecoder ("\"" ++ value ++ "\"") of
                            Ok conn ->
                                Decode.succeed (RequestConnection conn)

                            Err _ ->
                                Decode.fail ("Invalid connection: " ++ value)

                    "content-encoding" ->
                        case Decode.decodeString encodingDecoder ("\"" ++ value ++ "\"") of
                            Ok encoding ->
                                Decode.succeed (RequestContentEncoding encoding)

                            Err _ ->
                                Decode.fail ("Invalid content-encoding: " ++ value)

                    "content-language" ->
                        Decode.succeed (RequestContentLanguage value)

                    "content-length" ->
                        case String.toInt value of
                            Just length ->
                                Decode.succeed (RequestContentLength length)

                            Nothing ->
                                Decode.fail ("Invalid content-length: " ++ value)

                    "content-type" ->
                        case Decode.decodeString mimeTypeDecoder ("\"" ++ value ++ "\"") of
                            Ok mimeType ->
                                Decode.succeed (RequestContentType mimeType)

                            Err _ ->
                                Decode.fail ("Invalid content-type: " ++ value)

                    "cookie" ->
                        let
                            cookies =
                                String.split ";" value
                                    |> List.map String.trim
                                    |> List.filterMap
                                        (\cookieStr ->
                                            case String.split "=" cookieStr of
                                                name :: valueParts ->
                                                    Just { name = String.trim name, value = String.join "=" valueParts }

                                                [] ->
                                                    Nothing
                                        )
                        in
                        Decode.succeed (RequestCookie cookies)

                    "host" ->
                        Decode.succeed (RequestHost value)

                    "if-modified-since" ->
                        Decode.succeed (RequestIfModifiedSince value)

                    "if-none-match" ->
                        Decode.succeed (RequestIfNoneMatch value)

                    "origin" ->
                        Decode.succeed (RequestOrigin value)

                    "referer" ->
                        Decode.succeed (RequestReferrer value)

                    "referrer" ->
                        Decode.succeed (RequestReferrer value)

                    "user-agent" ->
                        Decode.succeed (RequestUserAgent value)

                    _ ->
                        Decode.succeed (RequestCustomHeader lowercaseName value)
            )


httpMethodFromString : String -> Result String HttpMethod
httpMethodFromString method =
    case String.toUpper method of
        "GET" ->
            Ok GET

        "POST" ->
            Ok POST

        "PUT" ->
            Ok PUT

        "PATCH" ->
            Ok PATCH

        "DELETE" ->
            Ok DELETE

        "HEAD" ->
            Ok HEAD

        "OPTIONS" ->
            Ok OPTIONS

        _ ->
            Err ("Unknown HTTP method: " ++ method)


httpMethodToString : HttpMethod -> String
httpMethodToString method =
    case method of
        GET ->
            "GET"

        POST ->
            "POST"

        PUT ->
            "PUT"

        PATCH ->
            "PATCH"

        DELETE ->
            "DELETE"

        HEAD ->
            "HEAD"

        OPTIONS ->
            "OPTIONS"


httpMethodDecoder : Decode.Decoder HttpMethod
httpMethodDecoder =
    Decode.string
        |> Decode.andThen
            (\methodStr ->
                case httpMethodFromString methodStr of
                    Ok method ->
                        Decode.succeed method

                    Err error ->
                        Decode.fail error
            )


httpMethodEncoder : HttpMethod -> Encode.Value
httpMethodEncoder method =
    Encode.string (httpMethodToString method)


parseImfFixdate : String -> Result String Time.Posix
parseImfFixdate str =
    let
        dayNameToInt dayName =
            case dayName of
                "Mon" ->
                    Ok 1

                "Tue" ->
                    Ok 2

                "Wed" ->
                    Ok 3

                "Thu" ->
                    Ok 4

                "Fri" ->
                    Ok 5

                "Sat" ->
                    Ok 6

                "Sun" ->
                    Ok 7

                _ ->
                    Err ("Invalid day name: " ++ dayName)

        monthNameToInt monthName =
            case monthName of
                "Jan" ->
                    Ok 1

                "Feb" ->
                    Ok 2

                "Mar" ->
                    Ok 3

                "Apr" ->
                    Ok 4

                "May" ->
                    Ok 5

                "Jun" ->
                    Ok 6

                "Jul" ->
                    Ok 7

                "Aug" ->
                    Ok 8

                "Sep" ->
                    Ok 9

                "Oct" ->
                    Ok 10

                "Nov" ->
                    Ok 11

                "Dec" ->
                    Ok 12

                _ ->
                    Err ("Invalid month name: " ++ monthName)

        parseTimePart timePart =
            case String.split ":" timePart of
                [ hourStr, minuteStr, secondStr ] ->
                    case ( String.toInt hourStr, String.toInt minuteStr, String.toInt secondStr ) of
                        ( Just hour, Just minute, Just second ) ->
                            if hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 && second >= 0 && second <= 60 then
                                Ok ( hour, minute, second )

                            else
                                Err "Time values out of range"

                        _ ->
                            Err "Invalid time format"

                _ ->
                    Err "Time must be in HH:MM:SS format"
    in
    case String.split " " str of
        [ dayNameWithComma, dayStr, monthStr, yearStr, timePart, timezone ] ->
            let
                dayName =
                    String.dropRight 1 dayNameWithComma

                -- Remove comma
            in
            if timezone /= "GMT" then
                Err "Timezone must be GMT"

            else
                case dayNameToInt dayName of
                    Err dayErr ->
                        Err dayErr

                    Ok _ ->
                        case String.toInt dayStr of
                            Nothing ->
                                Err "Invalid day number"

                            Just day ->
                                case monthNameToInt monthStr of
                                    Err monthErr ->
                                        Err monthErr

                                    Ok month ->
                                        case String.toInt yearStr of
                                            Nothing ->
                                                Err "Invalid year number"

                                            Just year ->
                                                case parseTimePart timePart of
                                                    Err timeErr ->
                                                        Err timeErr

                                                    Ok ( hour, minute, second ) ->
                                                        if day >= 1 && day <= 31 && year >= 1970 then
                                                            let
                                                                -- Convert to milliseconds since Unix epoch
                                                                -- This is a simplified calculation - for production use, consider using elm/time utilities
                                                                daysInMonth m y =
                                                                    case m of
                                                                        2 ->
                                                                            if modBy 4 y == 0 && (modBy 100 y /= 0 || modBy 400 y == 0) then
                                                                                29

                                                                            else
                                                                                28

                                                                        4 ->
                                                                            30

                                                                        6 ->
                                                                            30

                                                                        9 ->
                                                                            30

                                                                        11 ->
                                                                            30

                                                                        _ ->
                                                                            31

                                                                daysSinceEpoch =
                                                                    let
                                                                        yearsSince1970 =
                                                                            year - 1970

                                                                        leapDays =
                                                                            (year - 1 - 1968) // 4 - (year - 1 - 1900) // 100 + (year - 1 - 1600) // 400

                                                                        daysFromYears =
                                                                            yearsSince1970 * 365 + leapDays - ((1970 - 1 - 1968) // 4 - (1970 - 1 - 1900) // 100 + (1970 - 1 - 1600) // 400)

                                                                        daysFromMonths =
                                                                            List.range 1 (month - 1) |> List.map (\m -> daysInMonth m year) |> List.sum

                                                                        daysFromDay =
                                                                            day - 1
                                                                    in
                                                                    daysFromYears + daysFromMonths + daysFromDay

                                                                milliseconds =
                                                                    daysSinceEpoch * 24 * 60 * 60 * 1000 + hour * 60 * 60 * 1000 + minute * 60 * 1000 + second * 1000
                                                            in
                                                            if day <= daysInMonth month year then
                                                                Ok (Time.millisToPosix milliseconds)

                                                            else
                                                                Err "Day is invalid for the given month and year"

                                                        else
                                                            Err "Invalid day or year"

        _ ->
            Err "Invalid IMF-fixdate format. Expected: 'Day, DD Mon YYYY HH:MM:SS GMT'"


formatImfFixdate : Time.Posix -> String
formatImfFixdate posix =
    let
        utc =
            Time.utc

        weekday =
            Time.toWeekday utc posix

        day =
            Time.toDay utc posix

        month =
            Time.toMonth utc posix

        year =
            Time.toYear utc posix

        hour =
            Time.toHour utc posix

        minute =
            Time.toMinute utc posix

        second =
            Time.toSecond utc posix

        weekdayToString w =
            case w of
                Time.Mon ->
                    "Mon"

                Time.Tue ->
                    "Tue"

                Time.Wed ->
                    "Wed"

                Time.Thu ->
                    "Thu"

                Time.Fri ->
                    "Fri"

                Time.Sat ->
                    "Sat"

                Time.Sun ->
                    "Sun"

        monthToString m =
            case m of
                Time.Jan ->
                    "Jan"

                Time.Feb ->
                    "Feb"

                Time.Mar ->
                    "Mar"

                Time.Apr ->
                    "Apr"

                Time.May ->
                    "May"

                Time.Jun ->
                    "Jun"

                Time.Jul ->
                    "Jul"

                Time.Aug ->
                    "Aug"

                Time.Sep ->
                    "Sep"

                Time.Oct ->
                    "Oct"

                Time.Nov ->
                    "Nov"

                Time.Dec ->
                    "Dec"

        padZero n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n
    in
    weekdayToString weekday ++ ", " ++ padZero day ++ " " ++ monthToString month ++ " " ++ String.fromInt year ++ " " ++ padZero hour ++ ":" ++ padZero minute ++ ":" ++ padZero second ++ " GMT"


imfFixdateDecoder : Decode.Decoder Time.Posix
imfFixdateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case parseImfFixdate str of
                    Ok posix ->
                        Decode.succeed posix

                    Err err ->
                        Decode.fail ("Invalid IMF-fixdate: " ++ err)
            )


imfFixdateEncoder : Time.Posix -> Encode.Value
imfFixdateEncoder posix =
    Encode.string (formatImfFixdate posix)


httpMethodListToString : List HttpMethod -> String
httpMethodListToString methods =
    methods
        |> List.map httpMethodToString
        |> String.join ", "


cacheDirectiveToString : CacheDirective -> String
cacheDirectiveToString directive =
    case directive of
        MaxAge age ->
            "max-age=" ++ String.fromInt age

        SMaxAge age ->
            "s-maxage=" ++ String.fromInt age

        MustRevalidate ->
            "must-revalidate"

        ProxyRevalidate ->
            "proxy-revalidate"

        NoCache ->
            "no-cache"

        NoStore ->
            "no-store"

        Private ->
            "private"

        Public ->
            "public"

        MustUnderstand ->
            "must-understand"

        NoTransform ->
            "no-transform"

        Immutable ->
            "immutable"

        StaleWhileRevalidate age ->
            "stale-while-revalidate=" ++ String.fromInt age

        StaleIfError age ->
            "stale-if-error=" ++ String.fromInt age


cacheDirectiveListToString : List CacheDirective -> String
cacheDirectiveListToString directives =
    directives
        |> List.map cacheDirectiveToString
        |> String.join ", "


mimeTypeToString : MimeType -> String
mimeTypeToString mimeType =
    case mimeType of
        ApplicationFormUrlEncoded ->
            "application/x-www-form-urlencoded"

        ApplicationJson ->
            "application/json"

        ApplicationOctetStream ->
            "application/octet-stream"

        ApplicationPdf ->
            "application/pdf"

        ApplicationZip ->
            "application/zip"

        AudioMp3 ->
            "audio/mpeg"

        AudioWebm ->
            "audio/webm"

        CustomMime str ->
            str

        ImageAvif ->
            "image/avif"

        ImageGif ->
            "image/gif"

        ImageJpeg ->
            "image/jpeg"

        ImagePng ->
            "image/png"

        ImageSvg ->
            "image/svg+xml"

        ImageWebp ->
            "image/webp"

        MultipartFormData ->
            "multipart/form-data"

        TextCss ->
            "text/css"

        TextHtml ->
            "text/html"

        TextJavascript ->
            "text/javascript"

        TextPlain ->
            "text/plain"

        VideoMp4 ->
            "video/mp4"

        VideoWebm ->
            "video/webm"


encodingToString : Encoding -> String
encodingToString encoding =
    case encoding of
        Br ->
            "br"

        Deflate ->
            "deflate"

        Gzip ->
            "gzip"

        Identity ->
            "identity"

        Star ->
            "*"


contentDispositionToString : ContentDisposition -> String
contentDispositionToString disposition =
    case disposition of
        Inline maybeFilename ->
            case maybeFilename of
                Nothing ->
                    "inline"

                Just filename ->
                    "inline; filename=\"" ++ filename ++ "\""

        Attachment maybeFilename ->
            case maybeFilename of
                Nothing ->
                    "attachment"

                Just filename ->
                    "attachment; filename=\"" ++ filename ++ "\""

        FormData name maybeFilename ->
            case maybeFilename of
                Nothing ->
                    "form-data; name=\"" ++ name ++ "\""

                Just filename ->
                    "form-data; name=\"" ++ name ++ "\"; filename=\"" ++ filename ++ "\""


accessControlAllowOriginToString : AccessControlAllowOrigin -> String
accessControlAllowOriginToString origin =
    case origin of
        Wildcard ->
            "*"


xFrameOptionsToString : XFrameOptions -> String
xFrameOptionsToString option =
    case option of
        Deny ->
            "DENY"

        SameOrigin ->
            "SAMEORIGIN"


retryAfterToString : RetryAfter -> String
retryAfterToString retry =
    case retry of
        Seconds seconds ->
            String.fromInt seconds

        HttpDate posix ->
            formatImfFixdate posix


strictTransportSecurityToString : StrictTransportSecurity -> String
strictTransportSecurityToString (StrictTransportSecurity { maxAge, includeSubDomains, preload }) =
    let
        baseDirective =
            "max-age=" ++ String.fromInt maxAge

        withSubDomains =
            if includeSubDomains then
                baseDirective ++ "; includeSubDomains"

            else
                baseDirective

        withPreload =
            if preload then
                withSubDomains ++ "; preload"

            else
                withSubDomains
    in
    withPreload


cookieToString : { name : String, value : String } -> String
cookieToString { name, value } =
    name ++ "=" ++ value


mimeTypeDecoder : Decode.Decoder MimeType
mimeTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "application/x-www-form-urlencoded" ->
                        Decode.succeed ApplicationFormUrlEncoded

                    "application/json" ->
                        Decode.succeed ApplicationJson

                    "application/octet-stream" ->
                        Decode.succeed ApplicationOctetStream

                    "application/pdf" ->
                        Decode.succeed ApplicationPdf

                    "application/zip" ->
                        Decode.succeed ApplicationZip

                    "audio/mpeg" ->
                        Decode.succeed AudioMp3

                    "audio/webm" ->
                        Decode.succeed AudioWebm

                    "image/avif" ->
                        Decode.succeed ImageAvif

                    "image/gif" ->
                        Decode.succeed ImageGif

                    "image/jpeg" ->
                        Decode.succeed ImageJpeg

                    "image/png" ->
                        Decode.succeed ImagePng

                    "image/svg+xml" ->
                        Decode.succeed ImageSvg

                    "image/webp" ->
                        Decode.succeed ImageWebp

                    "multipart/form-data" ->
                        Decode.succeed MultipartFormData

                    "text/css" ->
                        Decode.succeed TextCss

                    "text/html" ->
                        Decode.succeed TextHtml

                    "text/javascript" ->
                        Decode.succeed TextJavascript

                    "text/plain" ->
                        Decode.succeed TextPlain

                    "video/mp4" ->
                        Decode.succeed VideoMp4

                    "video/webm" ->
                        Decode.succeed VideoWebm

                    _ ->
                        Decode.succeed (CustomMime str)
            )


encodingDecoder : Decode.Decoder Encoding
encodingDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "br" ->
                        Decode.succeed Br

                    "deflate" ->
                        Decode.succeed Deflate

                    "gzip" ->
                        Decode.succeed Gzip

                    "identity" ->
                        Decode.succeed Identity

                    "*" ->
                        Decode.succeed Star

                    _ ->
                        Decode.fail ("Unknown encoding: " ++ str)
            )


cacheDirectiveDecoder : Decode.Decoder CacheDirective
cacheDirectiveDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                if String.startsWith "max-age=" str then
                    case String.toInt (String.dropLeft 8 str) of
                        Just age ->
                            Decode.succeed (MaxAge age)

                        Nothing ->
                            Decode.fail ("Invalid max-age value: " ++ str)

                else if String.startsWith "s-maxage=" str then
                    case String.toInt (String.dropLeft 9 str) of
                        Just age ->
                            Decode.succeed (SMaxAge age)

                        Nothing ->
                            Decode.fail ("Invalid s-maxage value: " ++ str)

                else if String.startsWith "stale-while-revalidate=" str then
                    case String.toInt (String.dropLeft 23 str) of
                        Just age ->
                            Decode.succeed (StaleWhileRevalidate age)

                        Nothing ->
                            Decode.fail ("Invalid stale-while-revalidate value: " ++ str)

                else if String.startsWith "stale-if-error=" str then
                    case String.toInt (String.dropLeft 15 str) of
                        Just age ->
                            Decode.succeed (StaleIfError age)

                        Nothing ->
                            Decode.fail ("Invalid stale-if-error value: " ++ str)

                else
                    case str of
                        "must-revalidate" ->
                            Decode.succeed MustRevalidate

                        "proxy-revalidate" ->
                            Decode.succeed ProxyRevalidate

                        "no-cache" ->
                            Decode.succeed NoCache

                        "no-store" ->
                            Decode.succeed NoStore

                        "private" ->
                            Decode.succeed Private

                        "public" ->
                            Decode.succeed Public

                        "must-understand" ->
                            Decode.succeed MustUnderstand

                        "no-transform" ->
                            Decode.succeed NoTransform

                        "immutable" ->
                            Decode.succeed Immutable

                        _ ->
                            Decode.fail ("Unknown cache directive: " ++ str)
            )


authorizationDecoder : Decode.Decoder AuthorizationType
authorizationDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.split " " str of
                    "Basic" :: rest ->
                        Decode.succeed (Basic (String.join " " rest))

                    "Bearer" :: rest ->
                        Decode.succeed (Bearer (String.join " " rest))

                    scheme :: credentials ->
                        Decode.succeed (CustomAuth scheme (String.join " " credentials))

                    [] ->
                        Decode.fail "Empty authorization header"
            )


connectionTypeDecoder : Decode.Decoder ConnectionType
connectionTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.toLower str of
                    "close" ->
                        Decode.succeed Close

                    "keep-alive" ->
                        Decode.succeed KeepAlive

                    "upgrade" ->
                        Decode.succeed Upgrade

                    _ ->
                        Decode.fail ("Unknown connection type: " ++ str)
            )


parseItemWithQualityValue : Decode.Decoder a -> (a -> Float -> b) -> String -> Result String b
parseItemWithQualityValue itemDecoder constructor itemStr =
    let
        parts =
            String.split ";" itemStr

        mainPart =
            List.head parts |> Maybe.withDefault ""

        qValue =
            parts
                |> List.drop 1
                |> List.filterMap
                    (\part ->
                        if String.startsWith "q=" (String.trim part) then
                            String.dropLeft 2 (String.trim part)
                                |> String.toFloat

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault 1.0
    in
    case Decode.decodeString itemDecoder ("\"" ++ mainPart ++ "\"") of
        Ok parsedItem ->
            Ok (constructor parsedItem qValue)

        Err _ ->
            Err ("Failed to parse: " ++ itemStr)


parseCommaSeparatedQualityList : Decode.Decoder a -> (a -> Float -> b) -> String -> Decode.Decoder (List b)
parseCommaSeparatedQualityList itemDecoder constructor str =
    let
        items =
            String.split "," str
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        parseItem itemStr =
            parseItemWithQualityValue itemDecoder constructor itemStr
    in
    items
        |> List.map parseItem
        |> List.foldl
            (\result acc ->
                case ( result, acc ) of
                    ( Ok item, Ok list ) ->
                        Ok (item :: list)

                    ( Err err, _ ) ->
                        Err err

                    ( _, Err err ) ->
                        Err err
            )
            (Ok [])
        |> Result.map List.reverse
        |> (\result ->
                case result of
                    Ok list ->
                        Decode.succeed list

                    Err err ->
                        Decode.fail err
           )
