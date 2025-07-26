module Main exposing (main)

import RouteExample
import ElmRoot


main : ElmRoot.HttpServer () RouteExample.AppModel RouteExample.Error
main =
    ElmRoot.createServer RouteExample.exampleApp
