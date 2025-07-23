module RouteExample exposing (..)

import ElmRoot
import ElmRoot.Http
import ElmRoot.RouteParser as RouteParser
import ElmRoot.Types
import Json.Decode as Decode
import Json.Encode as Encode



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


getUserHandler : ElmRoot.Types.Request { id : Int } () -> ElmRoot.Types.Response GetUserResponse
getUserHandler request =
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
    { id = request.id
    , status = 200
    , body = user
    , headers = [ ElmRoot.Http.ResponseContentType ElmRoot.Http.ApplicationJson ]
    }


getPostHandler : ElmRoot.Types.Request { userId : Int, postId : Int } () -> ElmRoot.Types.Response String
getPostHandler request =
    let
        userIdStr =
            String.fromInt request.params.userId

        postIdStr =
            String.fromInt request.params.postId
    in
    { id = request.id
    , status = 200
    , body = "User " ++ userIdStr ++ " - Post " ++ postIdStr
    , headers = [ ElmRoot.Http.ResponseContentType ElmRoot.Http.TextPlain ]
    }


createUserHandler : ElmRoot.Types.Request () CreateUserRequest -> ElmRoot.Types.Response ()
createUserHandler request =
    { id = request.id
    , status = 201
    , body = ()
    , headers = []
    }


getUserRoute : ElmRoot.Types.RouteHandler
getUserRoute =
    ElmRoot.createRoute
        { method = ElmRoot.Http.GET
        , route =
            RouteParser.defineRoute
                "/user/:id"
                (RouteParser.succeed (\id -> { id = id }) |> RouteParser.required "id" RouteParser.int)
        , requestDecoder = ElmRoot.emptyRequestBody
        , responseEncoder = ElmRoot.jsonResponseBody encodeUser
        , handler = getUserHandler
        }


getPostRoute : ElmRoot.Types.RouteHandler
getPostRoute =
    ElmRoot.createRoute
        { method = ElmRoot.Http.GET
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


createUserRoute : ElmRoot.Types.RouteHandler
createUserRoute =
    ElmRoot.createRoute
        { method = ElmRoot.Http.POST
        , route =
            RouteParser.defineRoute "/user" RouteParser.noParams
        , requestDecoder = ElmRoot.jsonRequestBody decodeCreateUser
        , responseEncoder = ElmRoot.emptyResponseBody
        , handler = createUserHandler
        }


exampleApp : ElmRoot.Types.Application
exampleApp =
    { routes = [ getUserRoute, getPostRoute, createUserRoute ]
    , notFoundHandler =
        \request ->
            { id = request.id
            , status = 404
            , body = "{\"error\": \"Not Found\"}"
            , headers = []
            }
    }
