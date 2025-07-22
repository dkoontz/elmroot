port module Server exposing (HttpServer, accepted, badRequest, buildEmptyResponse, buildJsonResponse, buildTextResponse, corsOptionsHandler, createServer, created, forbidden, internalError, noContent, notFound, ok, okEmpty, okJson, okText, unauthorized, withContentType, withHeader, withHeaders)

import Http.Types
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import Types
import Url
import Url.Parser



-- Exposed Server functionality


createServer : Types.Application -> HttpServer
createServer application =
    Platform.worker
        { init = init application
        , update = update
        , subscriptions = subscriptions
        }


type alias HttpServer =
    Program () Model Msg



-- Elm lifecycle types and functions


type alias Model =
    { application : Types.Application }


type Msg
    = OnRequest HttpRequest
    | OnInvalidHttpFormat String


init : Types.Application -> flags -> ( Model, Cmd Msg )
init application _ =
    ( { application = application }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnRequest request ->
            let
                response =
                    model.application
                        |> executeRoute request
                        |> responseToHttp
                        |> httpResponseEncode
            in
            ( model
            , sendResponse response
            )

        OnInvalidHttpFormat errorMessage ->
            ( model
            , logging ("HttpRequest from Node was invalid: " ++ errorMessage)
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    httpRequest httpRequestDecoder


port httpRequest : (Decode.Value -> msg) -> Sub msg


port sendResponse : Encode.Value -> Cmd msg


port logging : String -> Cmd msg



-- Internal types


type alias NodeHttpRequest =
    { id : Types.RequestId
    , method : Http.Types.HttpMethod
    , url : Url.Url
    , body : String
    , headers : List Http.Types.Header
    }


type alias NodeHttpResponse =
    { id : Types.RequestId
    , status : Int
    , body : String
    , headers : List Types.Http.Header
    }



-- RUNNER UTILITIES


createResponse : Types.RequestId -> Int -> List Types.Http.Header -> responseBody -> Types.Response responseBody
createResponse requestId status headers body =
    { id = requestId
    , status = status
    , body = body
    , headers = headers
    }


responseToHttp : Types.Response String -> HttpResponse
responseToHttp response =
    { id = response.id
    , status = response.status
    , body = response.body
    , headers = response.headers
    }



-- ROUTE MATCHING ENGINE
-- Functions for finding and executing route handlers
-- Validate and decode request body based on RequestTypes


validateRequestBody : String -> Types.RequestTypes headerType bodyType -> Result String bodyType
validateRequestBody rawBody requestTypes =
    requestTypes.bodyDecoder rawBody



-- Create validated request from HTTP request and route configuration


createValidatedRequest : HttpRequest -> Types.RequestTypes headerType bodyType -> Result String bodyType
createValidatedRequest httpReq requestTypes =
    case validateRequestBody httpReq.body requestTypes of
        Ok validatedBody ->
            Ok validatedBody

        Err error ->
            Err ("Request validation failed: " ++ error)



-- Encode response body and set appropriate headers


encodeResponseBody : Types.Response bodyType -> Types.ResponseTypes headerType bodyType -> Types.Response String
encodeResponseBody response responseTypes =
    let
        encodedBody =
            responseTypes.bodyEncoder response.body

        contentTypeHeader =
            case responseTypes.bodyType of
                Types.JsonBody _ ->
                    [ ( "Content-Type", Types.applicationJson ) ]

                Types.TextBody _ ->
                    [ ( "Content-Type", Types.textPlain ) ]

                Types.BinaryBody _ ->
                    [ ( "Content-Type", Types.octetStream ) ]

                Types.EmptyBody ->
                    []

        updatedHeaders =
            contentTypeHeader ++ response.headers
    in
    { id = response.id
    , status = response.status
    , body = encodedBody
    , headers = updatedHeaders
    }



-- Match a request against a route handler
-- Match a request against a route handler with body validation


matchRoute : HttpRequest -> Types.RouteHandler routeParams requestHeaders requestBodyData responseHeaders responseData -> Maybe (Result String (Types.Response String))
matchRoute request (Types.RouteHandler config) =
    if request.method == config.method then
        case Url.Parser.parse config.route request.url of
            Just routeParams ->
                case createValidatedRequest request config.requestType of
                    Ok bodyData ->
                        let
                            -- Create request with both route params and body data
                            validatedRequest =
                                { id = request.id
                                , routeParams = routeParams
                                , bodyData = bodyData
                                , headers = request.headers
                                , body = request.body
                                }

                            -- Execute handler
                            response =
                                config.handler validatedRequest

                            -- Encode response body and set headers
                            encodedResponse =
                                encodeResponseBody response config.responseType
                        in
                        Just (Ok encodedResponse)

                    Err validationError ->
                        -- Return 400 error for validation failures
                        Just (Err validationError)

            Nothing ->
                Nothing

    else
        Nothing



-- Create a 400 Bad Request response with helpful error message


badRequestResponse : Types.RequestId -> String -> Types.Response String
badRequestResponse requestId errorMessage =
    { id = requestId
    , status = 400
    , body = "{\"error\": \"Bad Request\", \"message\": \"" ++ errorMessage ++ "\"}"
    , headers = [ ( "Content-Type", Types.applicationJson ) ]
    }



-- Parse request headers using the route's header decoder


parseRequestHeaders : Decode.Decoder requestHeaders -> HttpRequest -> Result String requestHeaders
parseRequestHeaders decoder request =
    let
        headersObject =
            Encode.object
                (List.map (\( key, value ) -> ( key, Encode.string value )) request.headers)
    in
    Decode.decodeValue decoder headersObject
        |> Result.mapError Decode.errorToString



-- Find the first matching route handler, handling validation errors


findMatchingRoute : HttpRequest -> List (Types.RouteHandler Types.NoRouteParams Types.EmptyHeaders Types.EmptyBodyData Types.EmptyHeaders String) -> Types.Response String
findMatchingRoute request routes =
    let
        attemptMatch route =
            case matchRoute request route of
                Just (Ok response) ->
                    Just response

                Just (Err error) ->
                    Just (badRequestResponse request.id error)

                Nothing ->
                    Nothing
    in
    routes
        |> List.filterMap attemptMatch
        |> List.head
        |> Maybe.withDefault (notFound request.id)



-- Execute a route handler with proper error handling and validation


executeRoute : HttpRequest -> Types.Application -> Types.Response String
executeRoute request app =
    -- Handle OPTIONS requests specially (for CORS)
    if request.method == Types.OPTIONS then
        corsOptionsHandler { id = request.id, routeParams = Types.NoRouteParams, bodyData = Types.EmptyBodyData, headers = request.headers, body = "" }

    else
        findMatchingRoute request app.routes



-- corsOptionsHandler : Types.Request Types.NoRouteParams Types.EmptyBodyData -> Types.Response String
-- corsOptionsHandler request =
--     createResponse request.id
--         200
--         ""
--         [ ( "Access-Control-Allow-Origin", "*" )
--         , ( "Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS" )
--         , ( "Access-Control-Allow-Headers", "Content-Type" )
--         ]
-- Helper functions for adding specific headers
-- withContentType : String -> Types.Response body -> Types.Response body
-- withContentType contentType response =
--     { response | headers = ( "Content-Type", contentType ) :: response.headers }
-- withHeader : String -> String -> Types.Response body -> Types.Response body
-- withHeader key value response =
--     { response | headers = ( key, value ) :: response.headers }
-- withHeaders : Types.Headers -> Types.Response body -> Types.Response body
-- withHeaders newHeaders response =
--     { response | headers = newHeaders ++ response.headers }
-- parseFormData : String -> List ( String, String )
-- parseFormData formString =
--     formString
--         |> String.split "&"
--         |> List.filterMap
--             (\pair ->
--                 case String.split "=" pair of
--                     [ key, value ] ->
--                         Just ( urlDecode key, urlDecode value )
--                     _ ->
--                         Nothing
--             )
-- Simple URL decode (in real app might want more robust implementation)
-- urlDecode : String -> String
-- urlDecode str =
--     str
--         |> String.replace "+" " "
--         |> String.replace "%20" " "
-- Helper functions for headers
-- getContentType : Types.Headers -> Maybe String
-- getContentType headers =
--     headers
--         |> List.filter (\( key, _ ) -> String.toLower key == "content-type")
--         |> List.head
--         |> Maybe.map Tuple.second
-- getContentLength : Types.Headers -> Maybe Int
-- getContentLength headers =
--     headers
--         |> List.filter (\( key, _ ) -> String.toLower key == "content-length")
--         |> List.head
--         |> Maybe.map Tuple.second
--         |> Maybe.andThen String.toInt
-- httpRequestDecoder : Decode.Value -> Msg
-- httpRequestDecoder value =
--     case Decode.decodeValue httpRequestDecode value of
--         Ok request ->
--             OnRequest request
--         Err err ->
--             OnInvalidHttpFormat (Decode.errorToString err)
-- decodeHttpMethod : Decode.Decoder Types.HttpMethod
-- decodeHttpMethod =
--     Decode.string
--         |> Decode.andThen
--             (\methodStr ->
--                 case httpMethodFromString methodStr of
--                     Ok method ->
--                         Decode.succeed method
--                     Err error ->
--                         Decode.fail error
--             )
-- decodeUrl : Decode.Decoder Url.Url
-- decodeUrl =
--     Decode.string
--         |> Decode.andThen
--             (\pathStr ->
--                 -- Create minimal URL structure for routing from path
--                 let
--                     -- Split path and query
--                     parts =
--                         String.split "?" pathStr
--                     path =
--                         parts
--                             |> List.head
--                             |> Maybe.withDefault "/"
--                     query =
--                         parts
--                             |> List.tail
--                             |> Maybe.andThen List.head
--                     url =
--                         { protocol = Url.Http
--                         , host = ""
--                         , port_ = Nothing
--                         , path = path
--                         , query = query
--                         , fragment = Nothing
--                         }
--                 in
--                 Decode.succeed url
--             )


httpResponseEncode : HttpResponse -> Encode.Value
httpResponseEncode response =
    Encode.object
        [ ( "id", Encode.string (Types.requestIdToString response.id) )
        , ( "status", Encode.int response.status )
        , ( "body", Encode.string response.body )
        , ( "headers", Encode.list (\( k, v ) -> Encode.list Encode.string [ k, v ]) response.headers )
        ]


httpRequestDecode : Decode.Decoder HttpRequest
httpRequestDecode =
    Decode.map5 HttpRequest
        (Decode.field "id" Types.decodeRequestId)
        (Decode.field "method" decodeHttpMethod)
        (Decode.field "path" decodeUrl)
        (Decode.field "body" Decode.string)
        (Decode.field "headers" (Decode.list Types.Http.decodeHeaders))
