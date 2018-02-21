module Request.Log exposing (Level(..), LogMessage, logError, logHttpError, logMessage)

import Http
import Json.Encode as Encode
import Ports
import Util exposing ((=>))


logHttpError : Http.Error -> Cmd msg
logHttpError httpError =
    let
        message =
            LogMessage (toString httpError) Error
    in
    encodeMessage message
        |> Encode.encode 0
        |> Ports.log


logError : String -> Cmd msg
logError messageString =
    let
        message =
            LogMessage messageString Error
    in
    encodeMessage message
        |> Encode.encode 0
        |> Ports.log


logMessage : LogMessage -> Cmd msg
logMessage message =
    encodeMessage message
        |> Encode.encode 0
        |> Ports.log


encodeMessage : LogMessage -> Encode.Value
encodeMessage logMessage =
    Encode.object
        [ "message"
            => Encode.string logMessage.message
        , "level"
            => encodeLevel logMessage.level
        ]


encodeLevel : Level -> Encode.Value
encodeLevel level =
    case level of
        Information ->
            Encode.string "Information"

        Warning ->
            Encode.string "Warning"

        Error ->
            Encode.string "Error"


type Level
    = Information
    | Warning
    | Error


type alias LogMessage =
    { message : String
    , level : Level
    }
