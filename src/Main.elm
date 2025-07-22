module Main exposing (main)

import App
import Server


main : Server.HttpServer
main =
    Server.createServer App.exampleApp
