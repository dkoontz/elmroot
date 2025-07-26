module ElmRoot.ErrorHelpers exposing (httpErrorToResponse, taskPortErrorToResponse)

import ElmRoot.Types
import Http
import TaskPort


taskPortErrorToResponse : ElmRoot.Types.RequestId -> TaskPort.Error -> ElmRoot.Types.Response String
taskPortErrorToResponse requestId error =
    case error of
        TaskPort.InteropError _ ->
            { id = requestId
            , status = 500
            , body = "{\"error\": \"Internal Server Error\", \"message\": \"Port communication failed\"}"
            , headers = []
            }

        TaskPort.JSError _ ->
            { id = requestId
            , status = 500
            , body = "{\"error\": \"Internal Server Error\", \"message\": \"JavaScript error in port\"}"
            , headers = []
            }


httpErrorToResponse : ElmRoot.Types.RequestId -> Http.Error -> ElmRoot.Types.Response String
httpErrorToResponse requestId httpError =
    case httpError of
        Http.BadUrl url ->
            { id = requestId
            , status = 400
            , body = "{\"error\": \"Bad Request\", \"message\": \"Invalid URL: " ++ url ++ "\"}"
            , headers = []
            }

        Http.Timeout ->
            { id = requestId
            , status = 504
            , body = "{\"error\": \"Gateway Timeout\", \"message\": \"Request timed out\"}"
            , headers = []
            }

        Http.NetworkError ->
            { id = requestId
            , status = 503
            , body = "{\"error\": \"Service Unavailable\", \"message\": \"Network error occurred\"}"
            , headers = []
            }

        Http.BadStatus statusCode ->
            { id = requestId
            , status = statusCode
            , body = "{\"error\": \"HTTP Error\", \"message\": \"Server returned status " ++ String.fromInt statusCode ++ "\"}"
            , headers = []
            }

        Http.BadBody message ->
            { id = requestId
            , status = 502
            , body = "{\"error\": \"Bad Gateway\", \"message\": \"Invalid response body: " ++ message ++ "\"}"
            , headers = []
            }
