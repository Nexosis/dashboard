module Data.Model exposing (Algorithm, ModelData, ModelList, PredictionResult, decodeModel, decodeModelList, decodePredictions)

import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Data.DisplayDate exposing (dateDecoder)
import Data.Link exposing (Link, linkDecoder)
import Data.Message exposing (Message, decodeMessage)
import Data.PredictionDomain exposing (PredictionDomain, decodePredictionDomain)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Time.ZonedDateTime exposing (ZonedDateTime)


type alias Algorithm =
    { name : String
    , description : String
    , key : String
    }


type alias ModelData =
    { modelId : String
    , sessionId : String
    , predictionDomain : PredictionDomain
    , dataSourceName : String
    , columns : List ColumnMetadata
    , createdDate : ZonedDateTime
    , algorithm : Algorithm
    , modelName : Maybe String
    , lastUsedDate : Maybe ZonedDateTime
    , metrics : Dict String Float
    , links : List Link
    }


type alias ModelList =
    { items : List ModelData
    , pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    }


type alias PredictionResult =
    { data : List (Dict String String)
    , messages : List Message
    }


decodeModelList : Decoder ModelList
decodeModelList =
    decode ModelList
        |> required "items" (Decode.list decodeModel)
        |> required "pageNumber" Decode.int
        |> required "totalPages" Decode.int
        |> required "pageSize" Decode.int
        |> required "totalCount" Decode.int


decodeAlgorithm : Decoder Algorithm
decodeAlgorithm =
    decode Algorithm
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "key" Decode.string


decodeModel : Decoder ModelData
decodeModel =
    decode ModelData
        |> required "modelId" Decode.string
        |> required "sessionId" Decode.string
        |> required "predictionDomain" decodePredictionDomain
        |> required "dataSourceName" Decode.string
        |> required "columns" decodeColumnMetadata
        |> required "createdDate" dateDecoder
        |> required "algorithm" decodeAlgorithm
        |> optional "modelName" (Decode.map Just string) Nothing
        |> optional "lastUsedDate" (Decode.map Just dateDecoder) Nothing
        |> required "metrics" (Decode.dict float)
        |> required "links" (Decode.list linkDecoder)


decodePredictions : Decoder PredictionResult
decodePredictions =
    decode PredictionResult
        |> required "data" (list <| dict string)
        |> required "messages" (list decodeMessage)
