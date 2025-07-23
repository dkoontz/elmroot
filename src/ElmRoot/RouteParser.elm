module ElmRoot.RouteParser exposing (RouteParser, defineRoute, int, noParams, optional, required, string, succeed)

import Dict exposing (Dict)


defineRoute : String -> RouteParser a -> (String -> Maybe (Result String a))
defineRoute routeDefinition parser path =
    matchRoute routeDefinition path
        |> Maybe.map (run parser)


noParams : RouteParser ()
noParams =
    RouteParser (\_ -> Ok ())


type RouteParser a
    = RouteParser (Dict String String -> Result String a)


succeed : a -> RouteParser a
succeed value =
    RouteParser (\_ -> Ok value)


required : String -> (String -> Result String b) -> RouteParser (b -> a) -> RouteParser a
required paramName parser (RouteParser parserFn) =
    RouteParser <|
        \dict ->
            case Dict.get paramName dict of
                Nothing ->
                    Err ("Required parameter '" ++ paramName ++ "' was not found")

                Just paramValue ->
                    case parser paramValue of
                        Err parseErr ->
                            Err ("Parameter '" ++ paramName ++ "': " ++ parseErr)

                        Ok parsedValue ->
                            case parserFn dict of
                                Err err ->
                                    Err err

                                Ok fn ->
                                    Ok (fn parsedValue)


optional : String -> (String -> Result String b) -> b -> RouteParser (b -> a) -> RouteParser a
optional paramName parser defaultValue (RouteParser parserFn) =
    RouteParser <|
        \dict ->
            let
                parsedValue =
                    case Dict.get paramName dict of
                        Nothing ->
                            Ok defaultValue

                        Just paramValue ->
                            case parser paramValue of
                                Err parseErr ->
                                    Err ("Parameter '" ++ paramName ++ "': " ++ parseErr)

                                Ok value ->
                                    Ok value
            in
            case parsedValue of
                Err err ->
                    Err err

                Ok value ->
                    case parserFn dict of
                        Err err ->
                            Err err

                        Ok fn ->
                            Ok (fn value)


run : RouteParser a -> Dict String String -> Result String a
run (RouteParser parserFn) dict =
    parserFn dict



-- Helper parsers for common types


int : String -> Result String Int
int str =
    case String.toInt str of
        Just value ->
            Ok value

        Nothing ->
            Err ("could not convert string '" ++ str ++ "' to an Int")


string : String -> Result String String
string str =
    Ok str


matchRoute : String -> String -> Maybe (Dict String String)
matchRoute routeDefinition path =
    let
        pathParts =
            String.split "/" path
                |> List.filter (\part -> part /= "")

        routeDefinitionParts =
            String.split "/" routeDefinition
                |> List.filter (\part -> part /= "")

        zip =
            List.map2 Tuple.pair
    in
    if List.length pathParts /= List.length routeDefinitionParts then
        Nothing

    else
        zip pathParts routeDefinitionParts
            |> List.foldl
                (\( pathPart, routePart ) maybeDict ->
                    case maybeDict of
                        Nothing ->
                            Nothing

                        Just dict ->
                            if String.startsWith ":" routePart then
                                let
                                    paramName =
                                        String.dropLeft 1 routePart
                                in
                                Just (Dict.insert paramName pathPart dict)

                            else if pathPart == routePart then
                                Just dict

                            else
                                Nothing
                )
                (Just Dict.empty)
