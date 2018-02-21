module Data.PredictionDomain exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, float, int, list, string, succeed)

type PredictionDomain = Regression
    | Classification
    | Forecast
    | Impact
    | Anomalies


decodePredictionDomain : Decoder PredictionDomain
decodePredictionDomain = 
    string |> andThen 
                (\n -> 
                    case n of
                        "regression" -> succeed Regression
                        "classification" -> succeed Classification
                        "forecast" -> succeed Forecast
                        "anomalies" -> succeed Anomalies
                        "impact" -> succeed Impact
                        unknown -> fail <| "Unknown prediction domain: " ++ unknown
                ) 