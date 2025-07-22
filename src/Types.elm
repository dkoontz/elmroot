module Types exposing (Application, HttpBody(..), NodeHttpRequest, Request, RequestId, Response, RouteConfig, RouteHandler(..), decodeRequestId, requestIdFromString, requestIdToString)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url
import Url.Parser


type alias Application =
    { routes : List RouteHandler
    , notFoundHandler : NodeHttpRequest -> Response String
    }


type HttpBody bodyType
    = JsonBody bodyType
    | TextBody String
    | EmptyBody


{-| This is an opaque type containing the id passed in from Node. By remaining opaque
we can ensure that the id is valid when returned as part of the response.
-}
type RequestId
    = RequestId String


decodeRequestId : Decode.Decoder RequestId
decodeRequestId =
    Decode.map RequestId Decode.string


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
    , headers : List Http.ResponseHeader
    }


type alias Request route requestBodyData =
    { id : RequestId
    , route : route
    , body : requestBodyData
    , headers : List Http.RequestHeader
    }


type alias NodeHttpRequest =
    { id : RequestId
    , method : Http.HttpMethod
    , url : Url.Url
    , body : String
    , headers : List Http.RequestHeader
    }


type RouteHandler
    = RouteHandler
        { method : Http.HttpMethod
        , matcher : NodeHttpRequest -> Maybe (Result String (Response String))
        }


type alias RouteConfig route requestBody responseBody =
    { method : Http.HttpMethod
    , path : Url.Parser.Parser (route -> route) route
    , requestDecoder : String -> Result String requestBody
    , responseEncoder : responseBody -> String
    , handler : Request route requestBody -> Response responseBody
    }
