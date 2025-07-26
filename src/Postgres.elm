module Postgres exposing (..)

import Json.Decode
import Json.Encode
import TaskPort


executeSqlQuery : Json.Decode.Decoder a -> { query : String, values : List String } -> TaskPort.Task a
executeSqlQuery decoder =
    TaskPort.call
        { function = "executeSqlQuery"
        , valueDecoder = decoder
        , argsEncoder =
            \arg ->
                Json.Encode.object
                    [ ( "query", Json.Encode.string arg.query )
                    , ( "values", Json.Encode.list Json.Encode.string arg.values )
                    ]
        }



-- type Msg = GotWidgetName (TaskPort.Result String)
-- Task.attempt GotWidgetName <| getWidgetNameByIndex 0
