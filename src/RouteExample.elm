module RouteExample exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Server
import Types
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


getUserHandler : Types.Request Route () -> Types.Response GetUserResponse
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
    , headers = [ Http.ResponseContentType Http.ApplicationJson ]
    }


createUserHandler : Types.Request Route CreateUserRequest -> Types.Response ()
createUserHandler request =
    -- In real app, would save to database using request.body
    { id = request.id
    , status = 201
    , body = ()
    , headers = []
    }


getUserRoute : Types.RouteHandler
getUserRoute =
    Server.createRoute
        { method = Http.GET
        , path = Parser.s "user" </> Parser.int |> Parser.map GetUser
        , requestDecoder = Server.emptyRequestBody
        , responseEncoder = Server.jsonResponseBody encodeUser
        , handler = getUserHandler
        }


createUserRoute : Types.RouteHandler
createUserRoute =
    Server.createRoute
        { method = Http.POST
        , path = Parser.s "users" |> Parser.map CreateUser
        , requestDecoder = Server.jsonRequestBody decodeCreateUser
        , responseEncoder = Server.emptyResponseBody
        , handler = createUserHandler
        }



-- Application with routes


exampleApp : Types.Application
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
