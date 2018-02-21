module Data.Model exposing (Algorithm, ModelData, ModelList, decodeModelList)

import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Time.DateTime exposing (DateTime, fromISO8601)
import Data.PredictionDomain exposing (PredictionDomain, decodePredictionDomain)

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
    , createdDate : DateTime
    , algorithm : Algorithm
    , metrics : Dict String Float
    }


type alias ModelList =
    { items : List ModelData
    , pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    }


stringToDate : Decoder DateTime
stringToDate =
    string
        |> andThen
            (\val ->
                case fromISO8601 val of
                    Err err ->
                        fail err

                    Ok datetime ->
                        succeed datetime
            )


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
        |> required "createdDate" stringToDate
        |> required "algorithm" decodeAlgorithm
        |> required "metrics" (Decode.dict float)
