module Data.Metric exposing (..)

import Json.Decode exposing (Decoder, Value, field, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias Metric =
    { key : String
    , name : String
    , description : String
    }


decodeMetricList : Decoder (List Metric)
decodeMetricList =
    field "metrics" (list decodeMetric)


decodeMetric : Decoder Metric
decodeMetric =
    decode Metric
        |> required "key" string
        |> required "name" string
        |> required "description" string
