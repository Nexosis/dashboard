module Request.Token exposing (renewAccessToken)

import Data.Config as Config exposing (Config)
import Http
import HttpBuilder exposing (post, toRequest, withCredentials, withExpectJson)


renewAccessToken : Config -> Http.Request Config.TokenResponse
renewAccessToken { renewalUrl } =
    renewalUrl
        |> post
        |> withExpectJson Config.tokenResponseDecoder
        |> withCredentials
        |> toRequest
