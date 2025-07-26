module RouteExample exposing (..)

import ElmRoot
import ElmRoot.ErrorHelpers
import ElmRoot.Http as ElmRoot
import ElmRoot.RouteParser as RouteParser
import ElmRoot.Types as ElmRoot
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import TaskPort



-- Example types for a user API


type alias GetUserResponse =
    { id : Int
    , firstName : String
    , lastName : String
    , email : String
    }


type alias CreateUserRequest =
    { firstName : String
    , lastName : String
    , email : String
    }


type Error
    = TaskPort TaskPort.Error
    | Http Http.Error
    | ValidationError String



-- JSON encoders/decoders


encodeUser : GetUserResponse -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "id", Encode.int user.id )
        , ( "firstName", Encode.string user.firstName )
        , ( "lastName", Encode.string user.lastName )
        , ( "email", Encode.string user.email )
        ]


decodeCreateUser : Decode.Decoder CreateUserRequest
decodeCreateUser =
    Decode.map3 CreateUserRequest
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.field "email" Decode.string)


getUserHandler : AppModel -> ElmRoot.Request { id : Int } () -> Task.Task Error (ElmRoot.Response GetUserResponse)
getUserHandler _ request =
    let
        userId =
            request.params.id
    in
    if userId <= 0 then
        Task.fail (ValidationError "User ID must be positive")

    else
        let
            user =
                { id = userId
                , firstName = "John"
                , lastName = "Doe"
                , email = "john.doe@example.com"
                }
        in
        Task.succeed
            { id = request.id
            , status = 200
            , body = user
            , headers = [ ElmRoot.ResponseContentType ElmRoot.ApplicationJson ]
            }


getPostHandler : AppModel -> ElmRoot.Request { userId : Int, postId : Int } () -> Task.Task Error (ElmRoot.Response String)
getPostHandler _ request =
    let
        userId =
            request.params.userId

        postId =
            request.params.postId
    in
    if userId <= 0 || postId <= 0 then
        Task.fail (ValidationError "User ID and Post ID must be positive")

    else
        let
            userIdStr =
                String.fromInt userId

            postIdStr =
                String.fromInt postId
        in
        Task.succeed
            { id = request.id
            , status = 200
            , body = "User " ++ userIdStr ++ " - Post " ++ postIdStr
            , headers = [ ElmRoot.ResponseContentType ElmRoot.TextPlain ]
            }


createUserHandler : AppModel -> ElmRoot.Request () CreateUserRequest -> Task.Task Error (ElmRoot.Response ())
createUserHandler _ request =
    let
        userData =
            request.body
    in
    if String.isEmpty userData.email then
        Task.fail (ValidationError "Email is required")

    else if String.isEmpty userData.firstName then
        Task.fail (ValidationError "First name is required")

    else if String.isEmpty userData.lastName then
        Task.fail (ValidationError "Last name is required")

    else
        Task.succeed
            { id = request.id
            , status = 201
            , body = ()
            , headers = []
            }


getUserRoute : ElmRoot.RouteHandler AppModel Error
getUserRoute =
    ElmRoot.createRoute
        { method = ElmRoot.GET
        , route =
            RouteParser.defineRoute
                "/user/:id"
                (RouteParser.succeed (\id -> { id = id }) |> RouteParser.required "id" RouteParser.int)
        , requestDecoder = ElmRoot.emptyRequestBody
        , responseEncoder = ElmRoot.jsonResponseBody encodeUser
        , handler = getUserHandler
        }


getPostRoute : ElmRoot.RouteHandler AppModel Error
getPostRoute =
    ElmRoot.createRoute
        { method = ElmRoot.GET
        , route =
            RouteParser.defineRoute "/user/:userId/post/:postId"
                (RouteParser.succeed (\userId postId -> { userId = userId, postId = postId })
                    |> RouteParser.required "userId" RouteParser.int
                    |> RouteParser.required "postId" RouteParser.int
                )
        , requestDecoder = ElmRoot.emptyRequestBody
        , responseEncoder = identity
        , handler = getPostHandler
        }


createUserRoute : ElmRoot.RouteHandler AppModel Error
createUserRoute =
    ElmRoot.createRoute
        { method = ElmRoot.POST
        , route =
            RouteParser.defineRoute "/user" RouteParser.noParams
        , requestDecoder = ElmRoot.jsonRequestBody decodeCreateUser
        , responseEncoder = ElmRoot.emptyResponseBody
        , handler = createUserHandler
        }


type alias AppModel =
    {}


handleError : ElmRoot.RequestId -> Error -> ElmRoot.Response String
handleError requestId error =
    case error of
        TaskPort taskPortError ->
            ElmRoot.ErrorHelpers.taskPortErrorToResponse requestId taskPortError

        Http httpError ->
            ElmRoot.ErrorHelpers.httpErrorToResponse requestId httpError

        ValidationError message ->
            { id = requestId
            , status = 422
            , body = "{\"error\": \"Validation Failed\", \"message\": \"" ++ message ++ "\"}"
            , headers = []
            }


exampleApp : ElmRoot.Application () AppModel Error
exampleApp =
    { routes = [ getUserRoute, getPostRoute, createUserRoute ]
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
