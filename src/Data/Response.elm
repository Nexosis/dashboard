module Data.Response exposing (GlobalMessage, Quota, Quotas, Response, ResponseError, decodeResponse, decodeXhrResponse, maxSize, responseErrorDecoder)

import AppRoutes exposing (Route)
import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, Value, andThen, decodeString, decodeValue, dict, fail, field, int, list, nullable, oneOf, string, succeed, value)
import Json.Decode.Extra exposing (doubleEncoded, withDefault)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, required)
import Nexosis.Types.Message exposing (Severity(..))


type alias Response =
    { status : Int
    , statusText : String
    , response : String
    , method : String
    , url : String
    , timestamp : String
    , messages : List GlobalMessage
    , quotas : Quotas
    }


type alias Quotas =
    { dataSets : Quota
    , sessions : Quota
    , predictions : Quota
    , dataSetSize : Quota
    }


type alias Quota =
    { allotted : Maybe Int
    , current : Maybe Int
    }


type alias ResponseError =
    { statusCode : Int
    , message : String
    , errorType : Maybe String
    , errorDetails : Dict String String
    }


type alias GlobalMessage =
    { severity : Severity
    , message : String
    , routeToResource : Maybe Route
    }


maxSize : Maybe Quotas -> Int
maxSize quotas =
    let
        default =
            1024 * 1024
    in
    Maybe.withDefault default (quotas |> Maybe.map .dataSetSize |> Maybe.map .allotted |> Maybe.withDefault Nothing)


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
                        field "response" (oneOf [ nestedMessagesDecoder route, succeed [] ])
                    )
            )
        |> required "quotas" decodeQuotas


decodeQuotas : Decoder Quotas
decodeQuotas =
    decode Quotas
        |> required "dataSets" decodeQuota
        |> required "sessions" decodeQuota
        |> required "predictions" decodeQuota
        |> required "dataSetSize" decodeQuota


decodeQuota : Decoder Quota
decodeQuota =
    decode Quota
        |> optional "allotted" (nullable int) Nothing
        |> optional "current" (nullable int) Nothing


nestedMessagesDecoder : Maybe Route -> Decoder (List GlobalMessage)
nestedMessagesDecoder route =
    doubleEncoded (field "messages" (list (decodeMessage route)) |> withDefault [])


decodeMessage : Maybe Route -> Decoder GlobalMessage
decodeMessage route =
    decode GlobalMessage
        |> required "severity" decodeSeverity
        |> required "message" string
        |> hardcoded route


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
        |> optional "errorDetails" (dict decodeErrorDetailValue) Dict.empty


decodeErrorDetailValue : Decoder String
decodeErrorDetailValue =
    oneOf [ string, list string |> andThen (\l -> succeed (toString l)), value |> andThen (\v -> succeed (toString v)) ]


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
