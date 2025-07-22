module ElmRoot.Types exposing (Application, NodeHttpRequest, Request, RequestId, Response, RouteConfig, RouteHandler(..), requestIdFromString, requestIdToString)

import ElmRoot.Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url
import Url.Parser


{-| Given a set of routes and a not-found handler, this type represents the HTTP server application.
Use @docs ElmRoot.createServer to create an instance of this type.
Use @docs ElmRoot.createRoute to create individual routes.

    exampleApp : ElmRoot.Types.Application
    exampleApp =
        { routes = [ getUserRoute, createUserRoute ]
        , notFoundHandler =
            \request ->
                { id = request.id
                , status = 404
                , body = "{\"error\": \"Not Found\"}"
                , headers = []
                }
        }

-}
type alias Application =
    { routes : List RouteHandler
    , notFoundHandler : NodeHttpRequest -> Response String
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


type alias Request route requestBodyData =
    { id : RequestId
    , route : route
    , body : requestBodyData
    , headers : List ElmRoot.Http.RequestHeader
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


type RouteHandler
    = RouteHandler
        { method : ElmRoot.Http.HttpMethod
        , matcher : NodeHttpRequest -> Maybe (Result String (Response String))
        }


type alias RouteConfig route requestBody responseBody =
    { method : ElmRoot.Http.HttpMethod
    , path : Url.Parser.Parser (route -> route) route
    , requestDecoder : String -> Result String requestBody
    , responseEncoder : responseBody -> String
    , handler : Request route requestBody -> Response responseBody
    }
