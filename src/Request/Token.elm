module Request.Token exposing (renewAccessToken)

import Data.Config as Config exposing (Config)
import Http
import HttpBuilder exposing (post, toRequest, withExpectJson)


renewAccessToken : Config -> Http.Request Config.NexosisToken
renewAccessToken { renewalUrl } =
    renewalUrl
        |> post
        |> withExpectJson Config.tokenDecoder
        |> toRequest
