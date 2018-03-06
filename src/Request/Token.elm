module Request.Token exposing (renewAccessToken)

import Data.Config as Config exposing (Config)
import Http
import HttpBuilder exposing (post, toRequest, withExpect)


renewAccessToken : Config -> Http.Request Config.NexosisToken
renewAccessToken { renewalUrl } =
    let
        expect =
            Config.tokenDecoder
                |> Http.expectJson
    in
    renewalUrl
        |> post
        |> withExpect expect
        |> toRequest
