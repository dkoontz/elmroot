module ElmRoot.Types exposing (Application, NodeHttpRequest, Request, RequestId, Response, RouteConfig, RouteHandler(..), requestIdFromString, requestIdToString)

import ElmRoot.Http
import Task
import Url


{-| Given a set of routes and functions for initializing a shared server state,
hadling errors and the not found route, this type represents the HTTP server application.
Use @docs ElmRoot.createServer to create an instance of this type.
Use @docs ElmRoot.createRoute to create individual routes.

    exampleApp : ElmRoot.Types.Application () AppModel Error
    exampleApp =
        { routes = [ getUserRoute, createUserRoute ]
        , notFoundHandler =
            \request ->
                { id = request.id
                , status = 404
                , body = "{\"error\": \"Not Found\"}"
                , headers = []
                }
        , errorHandler = handleError
        , init = \_ -> {}
        }

-}
type alias Application flags model appError =
    { routes : List (RouteHandler model appError)
    , notFoundHandler : NodeHttpRequest -> Response String
    , errorHandler : RequestId -> appError -> Response String
    , init : flags -> model
    }


{-| This is an opaque type containing the id passed in from Node. By remaining opaque
we can ensure that the id is valid when returned as part of the response.
-}
type RequestId
    = RequestId String


requestIdFromString : String -> RequestId
requestIdFromString id =
    RequestId id


requestIdToString : RequestId -> String
requestIdToString (RequestId id) =
    id


type alias Response responseTypes =
    { id : RequestId
    , status : Int
    , body : responseTypes
    , headers : List ElmRoot.Http.ResponseHeader
    }


type alias Request routeParams requestBodyData =
    { id : RequestId
    , params : routeParams
    , body : requestBodyData
    , headers : List ElmRoot.Http.RequestHeader
    , url : Url.Url
    }


{-| The raw HTTP request from Node.js, you should create your request handlsers with ElmRoot.createRoute
-}
type alias NodeHttpRequest =
    { id : RequestId
    , method : ElmRoot.Http.HttpMethod
    , url : Url.Url
    , body : String
    , headers : List ElmRoot.Http.RequestHeader
    }


type RouteHandler appModel appError
    = RouteHandler
        { method : ElmRoot.Http.HttpMethod
        , matcher : appModel -> NodeHttpRequest -> Maybe (Result String (Task.Task appError (Response String)))
        }


type alias RouteConfig appModel appError routeParams requestBody responseBody =
    { method : ElmRoot.Http.HttpMethod
    , route : String -> Maybe (Result String routeParams)
    , requestDecoder : String -> Result String requestBody
    , responseEncoder : responseBody -> String
    , handler : appModel -> Request routeParams requestBody -> Task.Task appError (Response responseBody)
    }
