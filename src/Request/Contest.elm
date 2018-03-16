module Request.Contest exposing (..)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.Contest exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpectJson)


get : Config -> String -> Http.Request Contest
get config sessionId =
    (config.baseUrl ++ "/sessions/" ++ sessionId ++ "/contest")
        |> HttpBuilder.get
        |> withExpectJson decodeContest
        |> withAuthorization config
        |> HttpBuilder.toRequest
