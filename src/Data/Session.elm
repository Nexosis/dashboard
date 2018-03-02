module Data.Session exposing (ResultInterval(..), SessionData, SessionList, decodeSession, decodeSessionList)

import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Data.Link exposing (..)
import Data.PredictionDomain exposing (..)
import Data.Status exposing (HistoryRecord, Status, decodeHistoryRecord, decodeStatus)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, field, float, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)


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
    , messages : List (Dict String String)
    , name : String
    , dataSourceName : String
    , targetColumn : String
    , links : List Link
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
        |> required "messages" (Decode.list (Decode.dict Decode.string))
        |> required "name" Decode.string
        |> required "dataSourceName" Decode.string
        |> optional "targetColumn" Decode.string ""
        |> required "links" (Decode.list linkDecoder)


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
                    "Hour" ->
                        succeed Hour

                    "Day" ->
                        succeed Day

                    "Week" ->
                        succeed Week

                    "Month" ->
                        succeed Month

                    "Year" ->
                        succeed Year

                    unknown ->
                        fail <| "Unknown columnType: " ++ unknown
            )
