module RouteExample exposing (..)

import ElmRoot
import ElmRoot.Http
import ElmRoot.Types
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Parser as Parser exposing ((</>))



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


type Route
    = GetUser Int
    | CreateUser



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



-- Route handlers


getUserHandler : ElmRoot.Types.Request Route () -> ElmRoot.Types.Response GetUserResponse
getUserHandler request =
    -- In real app, would fetch from database using the route
    let
        userId =
            case request.route of
                GetUser id ->
                    id

                -- This shouldn't happen due to routing
                _ ->
                    0

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


createUserHandler : ElmRoot.Types.Request Route CreateUserRequest -> ElmRoot.Types.Response ()
createUserHandler request =
    -- In real app, would save to database using request.body
    { id = request.id
    , status = 201
    , body = ()
    , headers = []
    }


getUserRoute : ElmRoot.Types.RouteHandler
getUserRoute =
    ElmRoot.createRoute
        { method = ElmRoot.Http.GET
        , path = Parser.s "user" </> Parser.int |> Parser.map GetUser
        , requestDecoder = ElmRoot.emptyRequestBody
        , responseEncoder = ElmRoot.jsonResponseBody encodeUser
        , handler = getUserHandler
        }


createUserRoute : ElmRoot.Types.RouteHandler
createUserRoute =
    ElmRoot.createRoute
        { method = ElmRoot.Http.POST
        , path = Parser.s "users" |> Parser.map CreateUser
        , requestDecoder = ElmRoot.jsonRequestBody decodeCreateUser
        , responseEncoder = ElmRoot.emptyResponseBody
        , handler = createUserHandler
        }



-- Application with routes


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
