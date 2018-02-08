module Data.Response exposing (Message, Response, Severity(..), decodeResponse, decodeXhrResponse)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value, andThen, decodeValue, fail, field, int, list, string, succeed)
import Json.Decode.Extra exposing (doubleEncoded, withDefault)
import Json.Decode.Pipeline exposing (decode, hardcoded, required)
import Route exposing (Route)


type alias Response =
    { status : Int
    , statusText : String
    , response : String
    , method : String
    , url : String
    , timestamp : String
    , messages : List Message
    }


type alias ResponseError =
    { statusCode : String
    , message : String
    , errorType : String
    , errorDetails : Maybe (Dict String String)
    }


type alias Message =
    { severity : Severity
    , message : String
    }


type Severity
    = Debug
    | Informational
    | Warning
    | Error


decodeXhrResponse : Value -> Result String Response
decodeXhrResponse value =
    decodeValue decodeResponse value


decodeResponse : Decoder Response
decodeResponse =
    decode Response
        |> required "status" int
        |> required "statusText" string
        |> required "response" string
        |> required "method" string
        |> required "url" string
        |> required "timestamp" string
        |> required "response" nestedMessagesDecoder


nestedMessagesDecoder : Decoder (List Message)
nestedMessagesDecoder =
    doubleEncoded (field "messages" (list decodeMessage) |> withDefault [])


decodeMessage : Decoder Message
decodeMessage =
    decode Message
        |> required "severity" decodeSeverity
        |> required "message" string


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
