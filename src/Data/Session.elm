module Data.Session exposing (ResultInterval(..), SessionData, SessionList, SessionResults, canPredictSession, decodeSession, decodeSessionList, decodeSessionResults, sessionIsCompleted)

import Data.Algorithm exposing (..)
import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Data.Link exposing (..)
import Data.Message exposing (..)
import Data.PredictionDomain exposing (..)
import Data.Status exposing (HistoryRecord, Status, decodeHistoryRecord, decodeStatus)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, field, float, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)


sessionIsCompleted : SessionData -> Bool
sessionIsCompleted session =
    case session.status of
        Data.Status.Completed ->
            True

        Data.Status.Failed ->
            True

        Data.Status.Cancelled ->
            True

        _ ->
            False


type alias SessionData =
    { sessionId : String
    , status : Status
    , predictionDomain : PredictionDomain
    , columns : List ColumnMetadata
    , availablePredictionIntervals : List String
    , startDate : Maybe String
    , endDate : Maybe String
    , resultInterval : Maybe ResultInterval
    , requestedDate : String
    , statusHistory : List HistoryRecord
    , extraParameters : Dict String String
    , messages : List Message
    , name : String
    , dataSourceName : String
    , targetColumn : Maybe String
    , links : List Link
    , modelId : Maybe String
    , algorithm : Maybe Algorithm
    }


type alias SessionResults =
    { metrics : Dict String Float
    , data : List (Dict String String)
    }


type alias SessionList =
    { items : List SessionData
    , pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    }


type ResultInterval
    = Hour
    | Day
    | Week
    | Month
    | Year


decodeSessionResults : Decode.Decoder SessionResults
decodeSessionResults =
    decode SessionResults
        |> required "metrics" (Decode.dict Decode.float)
        |> required "data" (Decode.list (Decode.dict Decode.string))


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
        |> optional "resultInterval" (Decode.map Just decodeResultInterval) Nothing
        |> required "requestedDate" Decode.string
        |> required "statusHistory" (Decode.list decodeHistoryRecord)
        |> required "extraParameters" (Decode.dict (Decode.oneOf [ Decode.string, Decode.bool |> Decode.andThen (\b -> succeed (toString b)) ]))
        |> required "messages" (Decode.list decodeMessage)
        |> required "name" Decode.string
        |> required "dataSourceName" Decode.string
        |> optional "targetColumn" (Decode.map Just string) Nothing
        |> required "links" (Decode.list linkDecoder)
        |> optional "modelId" (Decode.map Just string) Nothing
        |> optional "algorithm" (Decode.map Just decodeAlgorithm) Nothing


canPredictSession : SessionData -> Bool
canPredictSession session =
    case session.predictionDomain of
        Forecast ->
            False

        Impact ->
            False

        _ ->
            session.modelId /= Nothing


decodeSessionList : Decoder SessionList
decodeSessionList =
    decode SessionList
        |> required "items" (Decode.list decodeSession)
        |> required "pageNumber" Decode.int
        |> required "totalPages" Decode.int
        |> required "pageSize" Decode.int
        |> required "totalCount" Decode.int


decodeResultInterval : Decoder ResultInterval
decodeResultInterval =
    string
        |> andThen
            (\r ->
                case r of
                    "hour" ->
                        succeed Hour

                    "day" ->
                        succeed Day

                    "week" ->
                        succeed Week

                    "month" ->
                        succeed Month

                    "year" ->
                        succeed Year

                    unknown ->
                        fail <| "Unknown result interval: " ++ unknown
            )
