module Main exposing (main)

import RouteExample
import ElmRoot


main : ElmRoot.HttpServer
main =
    ElmRoot.createServer RouteExample.exampleApp
