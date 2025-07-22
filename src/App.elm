module App exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Server
import Types
import Url.Parser exposing ((</>))



-- EXAMPLE APPLICATION


exampleApp : Types.Application
exampleApp =
    { routes =
        [ homeRoute
        , healthRoute
        , infoRoute
        , textRoute
        , createUserRoute
        ]
    , notFoundHandler =
        \request ->
            Server.notFound request.id
    }



-- Route handlers for the example application


homeRoute : Types.RouteHandler Types.NoRouteParams Types.EmptyHeaders Types.EmptyBodyData Types.EmptyHeaders String
homeRoute =
    Types.RouteHandler
        { method = Types.GET
        , route = Url.Parser.top |> Url.Parser.map Types.NoRouteParams
        , requestType = Types.emptyRequest
        , responseType = Types.stringResponse
        , handler = \request -> Server.okText request.id "Hello from Elm backend!"
        }


healthRoute : Types.RouteHandler Types.NoRouteParams Types.EmptyHeaders Types.EmptyBodyData Types.EmptyHeaders String
healthRoute =
    Types.RouteHandler
        { method = Types.GET
        , route = Url.Parser.s "health" |> Url.Parser.map Types.NoRouteParams
        , requestType = Types.emptyRequest
        , responseType = Types.stringResponse
        , handler = \request -> Server.okText request.id "healthy"
        }



-- Echo route that echoes back the request body with URL parameters
-- Example route with URL parameters (simplified to match Application type)


greetingRoute : Types.RouteHandler String Types.EmptyHeaders Types.EmptyBodyData Types.EmptyHeaders String
greetingRoute =
    Types.RouteHandler
        { method = Types.GET
        , route = Url.Parser.s "api" </> Url.Parser.s "greet" </> Url.Parser.string
        , requestType = Types.emptyRequest
        , responseType = Types.stringResponse
        , handler =
            \request ->
                Server.okText request.id ("Hello, " ++ request.routeParams ++ "!")
        }



-- Example route that accepts JSON input and returns JSON output


type alias CreateUserRequest =
    { name : String
    , email : String
    }


createUserRequestDecoder : Decode.Decoder CreateUserRequest
createUserRequestDecoder =
    Decode.map2 CreateUserRequest
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)


type alias UserData =
    { id : Int
    , name : String
    , email : String
    , created : String -- ISO 8601 timestamp
    }


userDataEncoder : UserData -> Encode.Value
userDataEncoder userData =
    Encode.object
        [ ( "name", Encode.string userData.name )
        , ( "email", Encode.string userData.email )
        , ( "id", Encode.int 123 ) -- Mock ID
        , ( "created", Encode.string "2025-01-01T00:00:00Z" ) -- Mock timestamp
        ]



-- Simplified user creation route to match Application type


createUserRoute : Types.RouteHandler Types.NoRouteParams Types.JsonHeaders CreateUserRequest Types.JsonHeaders UserData
createUserRoute =
    Types.RouteHandler
        { method = Types.POST
        , route = Url.Parser.s "api" </> Url.Parser.s "users" |> Url.Parser.map Types.NoRouteParams
        , requestType = Types.jsonRequest createUserRequestDecoder
        , responseType = Types.jsonResponse userDataEncoder
        , handler =
            \request ->
                -- Create UserData from CreateUserRequest with mock additional fields
                let
                    userData =
                        { id = 123 -- Mock ID
                        , name = request.bodyData.name
                        , email = request.bodyData.email
                        , created = "2025-01-01T00:00:00Z" -- Mock timestamp
                        }
                in
                Server.created request.id userData userDataEncoder
        }



-- Example route that returns plain text


textRoute : Types.RouteHandler Types.NoRouteParams Types.EmptyHeaders Types.EmptyBodyData Types.EmptyHeaders String
textRoute =
    Types.RouteHandler
        { method = Types.GET
        , route = Url.Parser.s "api" </> Url.Parser.s "status" |> Url.Parser.map Types.NoRouteParams
        , requestType = Types.emptyRequest
        , responseType = Types.stringResponse
        , handler = \request -> Server.okText request.id "Server is running normally"
        }



-- Example route that returns empty response (204 No Content)


deleteRoute : Types.RouteHandler String Types.EmptyHeaders Types.EmptyBodyData Types.EmptyHeaders String
deleteRoute =
    Types.RouteHandler
        { method = Types.DELETE
        , route = Url.Parser.s "api" </> Url.Parser.s "items" </> Url.Parser.string
        , requestType = Types.emptyRequest
        , responseType = Types.stringResponse
        , handler =
            \request ->
                -- Simulate deleting item with extracted ID
                Server.okText request.id ("Item " ++ request.routeParams ++ " deleted successfully")
        }


infoRoute : Types.RouteHandler Types.NoRouteParams Types.EmptyHeaders Types.EmptyBodyData Types.EmptyHeaders String
infoRoute =
    Types.RouteHandler
        { method = Types.GET
        , route = Url.Parser.s "api" </> Url.Parser.s "info" |> Url.Parser.map Types.NoRouteParams
        , requestType = Types.emptyRequest
        , responseType = Types.stringResponse
        , handler = \request -> Server.okText request.id "API information"
        }
