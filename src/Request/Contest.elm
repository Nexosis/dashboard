module Request.Contest exposing (..)

import Data.Config as Config exposing (Config, withAppHeader)
import Data.Contest exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpectJson)
import Nexosis exposing (ClientConfig, withAuthorization)


get : Config -> String -> Http.Request Contest
get config sessionId =
    (config.clientConfig.url ++ "/sessions/" ++ sessionId ++ "/contest")
        |> HttpBuilder.get
        |> withExpectJson decodeContest
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest
