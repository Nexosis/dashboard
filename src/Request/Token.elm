module Request.Token exposing (renewAccessToken)

import Data.Config as Config exposing (Config)
import Data.Context exposing (TokenResponse, tokenResponseDecoder)
import Http
import HttpBuilder exposing (post, toRequest, withCredentials, withExpectJson)


renewAccessToken : Config -> Http.Request TokenResponse
renewAccessToken { renewalUrl } =
    renewalUrl
        |> post
        |> withExpectJson tokenResponseDecoder
        |> withCredentials
        |> toRequest
