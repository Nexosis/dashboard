module Data.Metric exposing (..)

import Json.Decode exposing (Decoder, Value, field, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import List.Extra as List


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


getMetricNameFromKey : List Metric -> String -> String
getMetricNameFromKey metricList key =
    let
        candidate =
            getMetricByKey metricList key
    in
    candidate.name


getMetricDescriptionFromKey : List Metric -> String -> String
getMetricDescriptionFromKey metricList key =
    let
        candidate =
            getMetricByKey metricList key
    in
    candidate.description


getMetricByKey : List Metric -> String -> Metric
getMetricByKey metricList key =
    let
        candidate =
            List.find (\a -> a.key == key) metricList
    in
    case candidate of
        Just metric ->
            metric

        Nothing ->
            Metric key "" ""
