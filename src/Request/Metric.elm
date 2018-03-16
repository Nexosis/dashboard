module Request.Metric exposing (..)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.Metric exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpectJson)


get : Config -> Http.Request (List Metric)
get config =
    (config.baseUrl ++ "/metrics")
        |> HttpBuilder.get
        |> withExpectJson decodeMetricList
        |> withAuthorization config
        |> HttpBuilder.toRequest
