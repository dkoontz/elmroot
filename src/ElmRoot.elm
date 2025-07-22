port module ElmRoot exposing (HttpServer, createRoute, createServer, emptyRequestBody, emptyResponseBody, jsonRequestBody, jsonResponseBody)

import ElmRoot.Http
import ElmRoot.Types
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import Url
import Url.Parser



-- Exposed Server functionality


createServer : ElmRoot.Types.Application -> HttpServer
createServer application =
    Platform.worker
        { init = init application
        , update = update
        , subscriptions = subscriptions
        }


type alias HttpServer =
    Program () Model Msg


createRoute : ElmRoot.Types.RouteConfig route requestBody responseBody -> ElmRoot.Types.RouteHandler
createRoute config =
    let
        processRequest { id, route, requestBody, headers, handler, responseEncoder } =
            -- Create the typed request
            let
                typedRequest =
                    { id = id
                    , route = route
                    , body = requestBody
                    , headers = headers
                    }

                -- Call the user's handler
                response =
                    handler typedRequest

                -- Encode the response body to String
                encodedBody =
                    responseEncoder response.body

                -- Create new response with encoded body
                encodedResponse : ElmRoot.Types.Response String
                encodedResponse =
                    { id = response.id
                    , status = response.status
                    , body = encodedBody
                    , headers = response.headers
                    }
            in
            encodedResponse
    in
    ElmRoot.Types.RouteHandler
        { method = config.method
        , matcher =
            \nodeRequest ->
                -- Check if HTTP method matches
                if nodeRequest.method == config.method then
                    -- Try to parse the URL path
                    case Url.Parser.parse config.path nodeRequest.url of
                        Just route ->
                            -- Try to decode the request body
                            case config.requestDecoder nodeRequest.body of
                                Ok requestBody ->
                                    Just
                                        (Ok
                                            (processRequest
                                                { id = nodeRequest.id
                                                , route = route
                                                , requestBody = requestBody
                                                , headers = nodeRequest.headers
                                                , handler = config.handler
                                                , responseEncoder = config.responseEncoder
                                                }
                                            )
                                        )

                                Err decodeError ->
                                    Just (Err decodeError)

                        Nothing ->
                            -- URL didn't match this route
                            Nothing

                else
                    -- HTTP method didn't match
                    Nothing
        }


emptyRequestBody : String -> Result String ()
emptyRequestBody =
    always (Ok ())


emptyResponseBody : () -> String
emptyResponseBody =
    always ""


jsonRequestBody : Decode.Decoder a -> String -> Result String a
jsonRequestBody decoder =
    Decode.decodeString decoder >> Result.mapError (\err -> "The Request body didn't match what I was expecting. " ++ Decode.errorToString err)


jsonResponseBody : (a -> Encode.Value) -> a -> String
jsonResponseBody encoder =
    encoder >> Encode.encode 0



-- Elm lifecycle types and functions


type alias Model =
    { application : ElmRoot.Types.Application }


type Msg
    = OnRequest ElmRoot.Types.NodeHttpRequest
    | OnInvalidHttpFormat ElmRoot.Types.RequestId String


init : ElmRoot.Types.Application -> flags -> ( Model, Cmd Msg )
init application _ =
    ( { application = application }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnRequest request ->
            let
                appResponse =
                    case tryRoutes request model.application.routes of
                        Just routeResponse ->
                            routeResponse

                        Nothing ->
                            model.application.notFoundHandler request

                response =
                    appResponse
                        |> responseToHttp
                        |> httpResponseEncode
            in
            ( model
            , sendResponse response
            )

        OnInvalidHttpFormat requestId errorMessage ->
            ( model
            , Cmd.batch
                [ notFoundResponse requestId |> httpResponseEncode |> sendResponse
                , logging ("HttpRequest from Node was invalid: " ++ errorMessage ++ " (RequestId: " ++ ElmRoot.Types.requestIdToString requestId ++ ")")
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    httpRequest
        (\value ->
            case Decode.decodeValue httpRequestDecodeWithId value of
                Ok request ->
                    OnRequest request

                Err error ->
                    case Decode.decodeValue (Decode.field "id" Decode.string) value of
                        Ok stringId ->
                            OnInvalidHttpFormat (ElmRoot.Types.requestIdFromString stringId) (Decode.errorToString error)

                        Err _ ->
                            OnInvalidHttpFormat (ElmRoot.Types.requestIdFromString "unknown") (Decode.errorToString error)
        )


port httpRequest : (Decode.Value -> msg) -> Sub msg


port sendResponse : Encode.Value -> Cmd msg


port logging : String -> Cmd msg



-- Internal types


type alias NodeHttpResponse =
    { id : ElmRoot.Types.RequestId
    , status : Int
    , body : String
    , headers : List ElmRoot.Http.ResponseHeader
    }



-- RUNNER UTILITIES


createResponse : ElmRoot.Types.RequestId -> Int -> List ElmRoot.Http.ResponseHeader -> responseBody -> ElmRoot.Types.Response responseBody
createResponse requestId status headers body =
    { id = requestId
    , status = status
    , body = body
    , headers = headers
    }


responseToHttp : ElmRoot.Types.Response String -> NodeHttpResponse
responseToHttp response =
    { id = response.id
    , status = response.status
    , body = response.body
    , headers = [] -- For now, empty headers. TODO: Convert from Http.RequestHeader to Http.ResponseHeader
    }


badRequestResponse : ElmRoot.Types.RequestId -> String -> ElmRoot.Types.Response String
badRequestResponse requestId errorMessage =
    { id = requestId
    , status = 400
    , body = "{\"error\": \"Bad Request\", \"message\": \"" ++ errorMessage ++ "\"}"
    , headers = []
    }


executeRoutes : ElmRoot.Types.NodeHttpRequest -> List ElmRoot.Types.RouteHandler -> ElmRoot.Types.Response String
executeRoutes request routes =
    case tryRoutes request routes of
        Just response ->
            response

        Nothing ->
            notFoundResponse request.id


tryRoutes : ElmRoot.Types.NodeHttpRequest -> List ElmRoot.Types.RouteHandler -> Maybe (ElmRoot.Types.Response String)
tryRoutes request routes =
    case routes of
        [] ->
            Nothing

        (ElmRoot.Types.RouteHandler config) :: remaining ->
            if request.method == config.method then
                case config.matcher request of
                    Just (Ok response) ->
                        Just response

                    Just (Err error) ->
                        Just (badRequestResponse request.id error)

                    Nothing ->
                        tryRoutes request remaining

            else
                tryRoutes request remaining


notFoundResponse : ElmRoot.Types.RequestId -> ElmRoot.Types.Response String
notFoundResponse requestId =
    { id = requestId
    , status = 404
    , body = "{\"error\": \"Not Found\"}"
    , headers = []
    }


httpResponseEncode : NodeHttpResponse -> Encode.Value
httpResponseEncode response =
    Encode.object
        [ ( "id", Encode.string (ElmRoot.Types.requestIdToString response.id) )
        , ( "status", Encode.int response.status )
        , ( "body", Encode.string response.body )
        , ( "headers", Encode.list ElmRoot.Http.encodeResponseHeader response.headers )
        ]


httpRequestDecode : ElmRoot.Types.RequestId -> Decode.Decoder ElmRoot.Types.NodeHttpRequest
httpRequestDecode requestId =
    Decode.map4 (\method url body headers -> ElmRoot.Types.NodeHttpRequest requestId method url body headers)
        (Decode.field "method" ElmRoot.Http.httpMethodDecoder)
        (Decode.field "url" decodeUrl)
        (Decode.field "body" Decode.string)
        (Decode.field "headers" (Decode.list ElmRoot.Http.decodeRequestHeader))


httpRequestDecodeWithId : Decode.Decoder ElmRoot.Types.NodeHttpRequest
httpRequestDecodeWithId =
    Decode.field "id" Decode.string
        |> Decode.andThen
            (\stringId ->
                let
                    requestId =
                        ElmRoot.Types.requestIdFromString stringId
                in
                httpRequestDecode requestId
            )


decodeUrl : Decode.Decoder Url.Url
decodeUrl =
    Decode.string
        |> Decode.andThen
            (\pathStr ->
                case Url.fromString pathStr of
                    Just url ->
                        Decode.succeed url

                    Nothing ->
                        Decode.fail ("Invalid URL: " ++ pathStr)
            )
