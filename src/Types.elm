module Types exposing (Application, AuthHeaders(..), EmptyBodyData(..), EmptyHeaders(..), EmptyToEmptyRoute, EmptyToJsonRoute, EmptyToTextRoute, HeaderType(..), Headers, HttpBody(..), HttpBodyDecoder, HttpBodyEncoder, HttpMethod(..), JsonHeaders(..), JsonToJsonRoute, NoRouteParams(..), Request, RequestId, RequestTypes, Response, ResponseTypes, RouteHandler(..), TextHeaders(..), applicationJson, decodeRequestId, emptyBody, emptyBodyDecoder, emptyBodyEncoder, emptyHeaders, emptyRequest, emptyResponse, jsonBodyDecoder, jsonBodyEncoder, jsonHeaders, jsonRequest, jsonResponse, octetStream, requestIdToString, stringResponse, textBodyDecoder, textBodyEncoder, textPlain, textResponse)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url
import Url.Parser


type alias Application =
    { routes : List RouteHandler
    , notFoundHandler : Request -> Response
    }


type HttpBody bodyType
    = JsonBody bodyType
    | TextBody String
    | EmptyBody


type alias RequestTypes headerType bodyType =
    { headerType : HeaderType headerType
    , bodyType : HttpBody bodyType
    , bodyDecoder : HttpBodyDecoder bodyType
    }


type alias ResponseTypes headerType bodyType =
    { headerType : HeaderType headerType
    , bodyType : HttpBody bodyType
    , bodyEncoder : HttpBodyEncoder bodyType
    }


{-| This is an opaque type containing the id passed in from Node. By remaining opaque
we can ensure that the id is valid when returned as part of the response.
-}
type RequestId
    = RequestId String


decodeRequestId : Decode.Decoder RequestId
decodeRequestId =
    Decode.map RequestId Decode.string


requestIdToString : RequestId -> String
requestIdToString (RequestId id) =
    id


type alias Response responseTypes =
    { id : RequestId
    , status : Int
    , body : responseTypes
    , headers : List Http.RequestHeader
    }


type alias Request routeParams requestBodyData =
    { id : RequestId
    , routeParams : routeParams
    , body : requestBodyData
    , headers : List Http.RequestHeader
    }


type alias RouteHandler routeParams requestHeaders requestBodyData responseHeaders responseData =
    { method : Http.HttpMethod
    , route : Url.Parser.Parser (routeParams -> routeParams) routeParams
    , requestType : RequestTypes requestHeaders requestBodyData
    , responseType : ResponseTypes responseHeaders responseData
    , handler : Request routeParams requestBodyData -> Response responseData
    }
