module Data.Session exposing (..)

import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Data.PredictionDomain exposing (..)
import Data.Status exposing (HistoryRecord, Status, decodeHistoryRecord, decodeStatus)
import Data.Algorithm exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, field, float, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)


sessionIsCompleted : SessionData -> Bool
sessionIsCompleted session =
    session.status == Completed ||
    session.status == Failed

type alias Message = {
    severity : Severity
    , message : String
}

type Severity
    = Debug
    | Informational
    | Warning
    | Error

type alias SessionData =
    { sessionId : String
    , status : Status
    , predictionDomain : PredictionDomain
    , columns : List ColumnMetadata
    , availablePredictionIntervals : List String
    , startDate : Maybe String
    , endDate : Maybe String
    , resultInterval : Maybe String
    , requestedDate : String
    , statusHistory : List HistoryRecord
    , extraParameters : Dict String String
    , messages : List Message
    , name : String
    , dataSourceName : String
    , targetColumn : Maybe String
    , modelId : Maybe String
    , algorithm : Maybe Algorithm
    }

type alias SessionResults = 
    { metrics : Dict String Float

    }


type alias SessionList =
    { items : List SessionData
    , pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    }

decodeSessionResults : Decode.Decoder SessionResults
decodeSessionResults =
    decode SessionResults
        |> required "metrics" (Decode.dict Decode.float)


decodeSession : Decode.Decoder SessionData
decodeSession =
    Json.Decode.Pipeline.decode SessionData
        |> required "sessionId" Decode.string
        |> required "status" decodeStatus
        |> required "predictionDomain" decodePredictionDomain
        |> required "columns" decodeColumnMetadata
        |> required "availablePredictionIntervals" (Decode.list Decode.string)
        |> optional "startDate" (Decode.map Just string) Nothing
        |> optional "endDate" (Decode.map Just string) Nothing
        |> optional "resultInterval" (Decode.map Just string) Nothing
        |> required "requestedDate" Decode.string
        |> required "statusHistory" (Decode.list decodeHistoryRecord)
        |> required "extraParameters" (Decode.dict (Decode.oneOf [ Decode.string, Decode.bool |> Decode.andThen (\b -> succeed (toString b)) ]))
        |> required "messages" (Decode.list decodeMessage)
        |> required "name" Decode.string
        |> required "dataSourceName" Decode.string
        |> optional "targetColumn" Decode.string ""
        |> optional "modelId" (Decode.map Just string) Nothing
        |> optional "algorithm" (Decode.map Just decodeAlgorithm) Nothing
        

canPredictSession : SessionData -> Bool
canPredictSession session = 
    case session.predictionDomain of
        Forecast -> False
        Impact -> False
        _ -> True


decodeMessage : Decoder Message
decodeMessage =
    decode Message
        |> required "severity"     decodeMessageSeverity 
        |> required "message" Decode.string


decodeMessageSeverity : Decoder Severity
decodeMessageSeverity =
    Decode.string
    |> andThen
        (\n ->
            case n of
                "debug" ->
                    succeed Debug

                "informational" ->
                    succeed Informational

                "warning" ->
                    succeed Warning

                "error" ->
                    succeed Error

                unknown ->
                    fail <| "Unknown Severity: " ++ unknown
        )

decodeSessionList : Decoder SessionList
decodeSessionList =
    decode SessionList
        |> required "items" (Decode.list decodeSession)
        |> required "pageNumber" Decode.int
        |> required "totalPages" Decode.int
        |> required "pageSize" Decode.int
        |> required "totalCount" Decode.int
