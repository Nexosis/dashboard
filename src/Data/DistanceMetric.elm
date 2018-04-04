module Data.DistanceMetric exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, dict, int, list, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias DistanceMetrics =
    { dataSourceName : String
    , modelId : String
    , sessionId : String
    , sessionName : String
    , data : Data
    , pageNumber : Int
    , pageSize : Int
    , totalCount : Int
    , totalPages : Int
    }


type alias DistanceValue =
    { anomaly : Float
    , distance : Float
    }


fromResponseString : String -> List DistanceValue
fromResponseString csvInput =
    [ DistanceValue 0 0 ]


fromDistanceMetrics : DistanceMetrics -> List DistanceValue
fromDistanceMetrics metrics =
    let
        toDistance : Maybe String -> Maybe String -> DistanceValue
        toDistance anomaly distance =
            DistanceValue (Maybe.withDefault "0" anomaly |> String.toFloat |> Result.withDefault 0) (Maybe.withDefault "0" distance |> String.toFloat |> Result.withDefault 0)
    in
    List.map (\d -> toDistance (Dict.get "anomaly" d) (Dict.get "mahalanobis_distance" d)) metrics.data


decodeDistanceMetrics : Decoder DistanceMetrics
decodeDistanceMetrics =
    decode DistanceMetrics
        |> required "dataSourceName" string
        |> required "modelId" string
        |> required "sessionId" string
        |> required "name" string
        |> required "data" decodeData
        |> required "pageNumber" int
        |> required "pageSize" int
        |> required "totalCount" int
        |> required "totalPages" int


type alias Data =
    List (Dict String String)


decodeData : Decoder Data
decodeData =
    list <|
        dict (oneOf [ string, succeed "" ])
