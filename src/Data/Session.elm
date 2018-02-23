module Data.Session exposing (..)

import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Data.PredictionDomain exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, field, float, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)


type SessionStatus
    = Requested
    | Started
    | Completed


type alias HistoryRecord =
    { date : String
    , status : SessionStatus
    }


type alias SessionData =
    { sessionId : String
    , status : SessionStatus
    , predictionDomain : PredictionDomain
    , columns : List ColumnMetadata
    , availablePredictionIntervals : List String
    , startDate : String
    , endDate : String
    , resultInterval : String
    , requestedDate : String
    , statusHistory : List HistoryRecord
    , extraParameters : Dict String String
    , messages : List (Dict String String)
    , name : String
    , dataSourceName : String
    , targetColumn : String
    }


type alias SessionList =
    { items : List SessionData
    , pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    }


decodeSession : Decode.Decoder SessionData
decodeSession =
    Json.Decode.Pipeline.decode SessionData
        |> required "sessionId" Decode.string
        |> required "status" decodeSessionStatus
        |> required "predictionDomain" decodePredictionDomain
        |> required "columns" decodeColumnMetadata
        |> required "availablePredictionIntervals" (Decode.list Decode.string)
        |> required "startDate" Decode.string
        |> required "endDate" Decode.string
        |> required "resultInterval" Decode.string
        |> required "requestedDate" Decode.string
        |> required "statusHistory" (Decode.list decodeHistoryRecord)
        |> required "extraParameters" (Decode.dict Decode.string)
        |> required "messages" (Decode.list (Decode.dict Decode.string))
        |> optional "name" Decode.string ""
        |> required "dataSourceName" Decode.string
        |> required "targetColumn" Decode.string


decodeSessionList : Decoder SessionList
decodeSessionList =
    decode SessionList
        |> required "items" (Decode.list decodeSession)
        |> required "pageNumber" Decode.int
        |> required "totalPages" Decode.int
        |> required "pageSize" Decode.int
        |> required "totalCount" Decode.int


decodeHistoryRecord : Decoder HistoryRecord
decodeHistoryRecord =
    map2 HistoryRecord
        (field "date" Decode.string)
        (field "status" decodeSessionStatus)


decodeSessionStatus : Decoder SessionStatus
decodeSessionStatus =
    Decode.string
        |> andThen
            (\n ->
                case n of
                    "requested" ->
                        succeed Requested

                    "started" ->
                        succeed Started

                    "completed" ->
                        succeed Completed

                    unknown ->
                        fail <| "Unknown session status: " ++ unknown
            )
