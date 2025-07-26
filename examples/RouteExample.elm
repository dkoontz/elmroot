module RouteExample exposing (..)

import ElmRoot
import ElmRoot.Http as ElmRoot
import ElmRoot.RouteParser as RouteParser
import ElmRoot.Types as ElmRoot
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


getUserHandler : AppModel -> ElmRoot.Request { id : Int } () -> TaskPort.Task (ElmRoot.Response GetUserResponse)
getUserHandler _ request =
    let
        userId =
            request.params.id

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


getPostHandler : AppModel -> ElmRoot.Request { userId : Int, postId : Int } () -> TaskPort.Task (ElmRoot.Response String)
getPostHandler _ request =
    let
        userIdStr =
            String.fromInt request.params.userId

        postIdStr =
            String.fromInt request.params.postId
    in
    Task.succeed
        { id = request.id
        , status = 200
        , body = "User " ++ userIdStr ++ " - Post " ++ postIdStr
        , headers = [ ElmRoot.ResponseContentType ElmRoot.TextPlain ]
        }


createUserHandler : AppModel -> ElmRoot.Request () CreateUserRequest -> TaskPort.Task (ElmRoot.Response ())
createUserHandler _ request =
    Task.succeed
        { id = request.id
        , status = 201
        , body = ()
        , headers = []
        }


getUserRoute : ElmRoot.RouteHandler AppModel
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


getPostRoute : ElmRoot.RouteHandler AppModel
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


createUserRoute : ElmRoot.RouteHandler AppModel
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


exampleApp : ElmRoot.Application () AppModel
exampleApp =
    { routes = [ getUserRoute, getPostRoute, createUserRoute ]
    , notFoundHandler =
        \request ->
            { id = request.id
            , status = 404
            , body = "{\"error\": \"Not Found\"}"
            , headers = []
            }
    , init = \_ -> {}
    }
