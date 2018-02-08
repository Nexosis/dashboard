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
    , routeToResource : Maybe Route
    }


type Severity
    = Debug
    | Informational
    | Warning
    | Error


decodeXhrResponse : String -> Value -> Result String Response
decodeXhrResponse baseUrl value =
    decodeValue (decodeResponse baseUrl) value


decodeResponse : String -> Decoder Response
decodeResponse baseUrl =
    decode Response
        |> required "status" int
        |> required "statusText" string
        |> required "response" string
        |> required "method" string
        |> required "url" string
        |> required "timestamp" string
        |> Json.Decode.Pipeline.custom
            (field "url" string
                |> andThen
                    (\url ->
                        let
                            route =
                                Route.fromApiUrl baseUrl url
                        in
                        field "response" (nestedMessagesDecoder route)
                    )
            )


nestedMessagesDecoder : Maybe Route -> Decoder (List Message)
nestedMessagesDecoder route =
    doubleEncoded (field "messages" (list (decodeMessage route)) |> withDefault [])


decodeMessage : Maybe Route -> Decoder Message
decodeMessage route =
    decode Message
        |> required "severity" decodeSeverity
        |> required "message" string
        |> hardcoded route


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
