module Postgres exposing (..)

import Json.Decode
import Json.Encode
import TaskPort


executeSqlQuery : String -> TaskPort.Task String
executeSqlQuery =
    TaskPort.call
        { function = "executeSqlQuery"
        , valueDecoder = Json.Decode.string
        , argsEncoder = Json.Encode.string
        }



-- type Msg = GotWidgetName (TaskPort.Result String)
-- Task.attempt GotWidgetName <| getWidgetNameByIndex 0
