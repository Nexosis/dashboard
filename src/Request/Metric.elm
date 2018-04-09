module Request.Metric exposing (..)

import Data.Config as Config exposing (Config, withAppHeader)
import Data.Metric exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpectJson)
import Nexosis exposing (ClientConfig, withAuthorization)


get : Config -> Http.Request (List Metric)
get config =
    (config.clientConfig.url ++ "/metrics")
        |> HttpBuilder.get
        |> withExpectJson decodeMetricList
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest
