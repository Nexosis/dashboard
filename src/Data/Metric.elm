module Data.Metric exposing (..)

import List.Extra as List
import Nexosis.Api.Metrics exposing (Metric)


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
