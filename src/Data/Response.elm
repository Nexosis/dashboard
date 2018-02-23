module Data.Response exposing (Message, Response, ResponseError, Severity(..), decodeResponse, decodeXhrResponse, responseErrorDecoder)

import AppRoutes exposing (Route)
import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, Value, andThen, decodeString, decodeValue, dict, fail, field, int, list, nullable, string, succeed)
import Json.Decode.Extra exposing (doubleEncoded, withDefault)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, required)


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
    { statusCode : Int
    , message : String
    , errorType : Maybe String
    , errorDetails : Dict String String
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
                                AppRoutes.fromApiUrl baseUrl url
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


responseErrorDecoder : Http.Response String -> ResponseError
responseErrorDecoder responseError =
    decodeString decodeResponseBodyError responseError.body
        |> Result.withDefault
            { statusCode = responseError.status.code
            , message = responseError.status.message
            , errorType = Nothing
            , errorDetails = Dict.empty
            }


decodeResponseBodyError : Decoder ResponseError
decodeResponseBodyError =
    decode ResponseError
        |> required "statusCode" int
        |> required "message" string
        |> optional "errorType" (nullable string) Nothing
        |> optional "errorDetails" (dict string) Dict.empty
