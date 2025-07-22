module Main exposing (main)

import RouteExample
import Server


main : Server.HttpServer
main =
    Server.createServer RouteExample.exampleApp
