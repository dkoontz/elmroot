module Main exposing (main)

import RouteExample
import ElmRoot


main : ElmRoot.HttpServer () RouteExample.AppModel
main =
    ElmRoot.createServer RouteExample.exampleApp
