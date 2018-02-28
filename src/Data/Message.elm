module Data.Message exposing (Message, Severity(..), decodeMessage, decodeSeverity)

import Json.Decode exposing (Decoder, andThen, fail, field, map2, string, succeed)


type alias Message =
    { severity : Severity
    , message : String
    }


type Severity
    = Debug
    | Informational
    | Warning
    | Error


decodeMessage : Decoder Message
decodeMessage =
    map2 Message
        (field "severity" decodeSeverity)
        (field "message" string)


decodeSeverity : Decoder Severity
decodeSeverity =
    string
        |> andThen
            (\severity ->
                case severity of
                    "debug" ->
                        succeed Debug

                    "informational" ->
                        succeed Informational

                    "warning" ->
                        succeed Warning

                    "error" ->
                        succeed Error

                    unknown ->
                        fail <| "Unknown message severity: " ++ unknown
            )
