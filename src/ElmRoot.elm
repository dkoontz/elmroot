port module ElmRoot exposing (HttpServer, createRoute, createServer, emptyRequestBody, emptyResponseBody, jsonRequestBody, jsonResponseBody, stringResponseBody)

import ElmRoot.Http
import ElmRoot.Types
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import Task
import Url



-- Exposed Server functionality


createServer : ElmRoot.Types.Application flags appModel appError -> HttpServer flags appModel appError
createServer application =
    Platform.worker
        { init = init application
        , update = update
        , subscriptions = subscriptions
        }


type alias HttpServer flags appModel appError =
    Program flags (Model flags appModel appError) (Msg appError)


createRoute : ElmRoot.Types.RouteConfig appModel appError routeParams requestBody responseBody -> ElmRoot.Types.RouteHandler appModel appError
createRoute config =
    let
        processRequest appModel { id, params, requestBody, headers, handler, responseEncoder, url } =
            -- Create the typed request
            let
                typedRequest =
                    { id = id
                    , params = params
                    , body = requestBody
                    , headers = headers
                    , url = url
                    }

                -- Call the user's handler
            in
            handler appModel typedRequest
                |> Task.map
                    (\response ->
                        -- Encode the response body to String
                        { id = response.id
                        , status = response.status
                        , body = responseEncoder response.body
                        , headers = response.headers
                        }
                    )
    in
    ElmRoot.Types.RouteHandler
        { method = config.method
        , matcher =
            \appModel nodeRequest ->
                -- Check if HTTP method matches
                if nodeRequest.method == config.method then
                    -- Try to parse the URL path
                    case config.route nodeRequest.url.path of
                        Just paramsResult ->
                            case paramsResult of
                                Err error ->
                                    Just (Err error)

                                Ok params ->
                                    -- Route params parsed ok, now try to decode the request body
                                    case config.requestDecoder nodeRequest.body of
                                        Ok requestBody ->
                                            Just
                                                (Ok
                                                    (processRequest appModel
                                                        { id = nodeRequest.id
                                                        , params = params
                                                        , requestBody = requestBody
                                                        , headers = nodeRequest.headers
                                                        , handler = config.handler
                                                        , responseEncoder = config.responseEncoder
                                                        , url = nodeRequest.url
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


stringResponseBody : String -> String
stringResponseBody =
    identity



-- Elm lifecycle types and functions


type alias Model flags appModel appError =
    { application : ElmRoot.Types.Application flags appModel appError
    , appModel : appModel
    }


type Msg appError
    = OnRequest ElmRoot.Types.NodeHttpRequest
    | OnInvalidHttpFormat ElmRoot.Types.RequestId String
    | RequestResult ElmRoot.Types.RequestId (Result appError (ElmRoot.Types.Response String))


init : ElmRoot.Types.Application flags appModel appError -> flags -> ( Model flags appModel appError, Cmd (Msg appError) )
init application flags =
    ( { application = application, appModel = application.init flags }, Cmd.none )


update : Msg appError -> Model flags appModel appError -> ( Model flags appModel appError, Cmd (Msg appError) )
update msg model =
    case msg of
        OnRequest request ->
            let
                appResponse =
                    case tryRoutes model.appModel request model.application.routes of
                        Just routeResponse ->
                            routeResponse

                        Nothing ->
                            Task.succeed (model.application.notFoundHandler request)

                appTask =
                    appResponse
                        |> Task.attempt (RequestResult request.id)
            in
            ( model
            , appTask
            )

        OnInvalidHttpFormat requestId errorMessage ->
            ( model
            , Cmd.batch
                [ notFoundResponse requestId |> nodeHttpResponseEncode |> sendResponse
                , logging ("HttpRequest from Node was invalid: " ++ errorMessage ++ " (RequestId: " ++ ElmRoot.Types.requestIdToString requestId ++ ")")
                ]
            )

        RequestResult requestId result ->
            case result of
                Ok response ->
                    ( model
                    , response |> responseToNodeHttp |> nodeHttpResponseEncode |> sendResponse
                    )

                Err error ->
                    let
                        errorResponse =
                            model.application.errorHandler requestId error
                    in
                    ( model
                    , Cmd.batch
                        [ errorResponse |> responseToNodeHttp |> nodeHttpResponseEncode |> sendResponse
                        , logging "Error processing request"
                        ]
                    )


subscriptions : Model flags appModel appError -> Sub (Msg appError)
subscriptions model =
    httpRequest
        (\value ->
            case Decode.decodeValue nodeHttpRequestDecodeWithId value of
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


responseToNodeHttp : ElmRoot.Types.Response String -> NodeHttpResponse
responseToNodeHttp response =
    { id = response.id
    , status = response.status
    , body = response.body
    , headers = [] -- For now, empty headers. TODO: Convert from Http.RequestHeader to Http.ResponseHeader
    }


badRequestResponse : ElmRoot.Types.RequestId -> String -> ElmRoot.Types.Response String
badRequestResponse requestId errorMessage =
    { id = requestId
    , status = 400
    , body = Encode.object [ ( "error", Encode.string "Bad Request" ), ( "message", Encode.string errorMessage ) ] |> Encode.encode 0
    , headers = []
    }


tryRoutes : appModel -> ElmRoot.Types.NodeHttpRequest -> List (ElmRoot.Types.RouteHandler appModel appError) -> Maybe (Task.Task appError (ElmRoot.Types.Response String))
tryRoutes appModel request routes =
    case routes of
        [] ->
            Nothing

        (ElmRoot.Types.RouteHandler config) :: remaining ->
            if request.method == config.method then
                case config.matcher appModel request of
                    Just (Ok response) ->
                        Just response

                    Just (Err error) ->
                        Just (Task.succeed (badRequestResponse request.id error))

                    Nothing ->
                        tryRoutes appModel request remaining

            else
                tryRoutes appModel request remaining


notFoundResponse : ElmRoot.Types.RequestId -> ElmRoot.Types.Response String
notFoundResponse requestId =
    { id = requestId
    , status = 404
    , body = Encode.object [ ( "error", Encode.string "NotFound" ) ] |> Encode.encode 0
    , headers = []
    }


nodeHttpResponseEncode : NodeHttpResponse -> Encode.Value
nodeHttpResponseEncode response =
    Encode.object
        [ ( "id", Encode.string (ElmRoot.Types.requestIdToString response.id) )
        , ( "status", Encode.int response.status )
        , ( "body", Encode.string response.body )
        , ( "headers", Encode.list ElmRoot.Http.encodeResponseHeader response.headers )
        ]


nodeHttpRequestDecode : ElmRoot.Types.RequestId -> Decode.Decoder ElmRoot.Types.NodeHttpRequest
nodeHttpRequestDecode requestId =
    Decode.map4 (\method url body headers -> ElmRoot.Types.NodeHttpRequest requestId method url body headers)
        (Decode.field "method" ElmRoot.Http.httpMethodDecoder)
        (Decode.field "url" decodeUrl)
        (Decode.field "body" Decode.string)
        (Decode.field "headers" (Decode.list ElmRoot.Http.decodeRequestHeader))


nodeHttpRequestDecodeWithId : Decode.Decoder ElmRoot.Types.NodeHttpRequest
nodeHttpRequestDecodeWithId =
    Decode.field "id" Decode.string
        |> Decode.andThen
            (\stringId ->
                let
                    requestId =
                        ElmRoot.Types.requestIdFromString stringId
                in
                nodeHttpRequestDecode requestId
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
