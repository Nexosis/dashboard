module Request.Token exposing (renewAccessToken)

import HttpBuilder exposing (post, withExpect, toRequest)
import Data.Config as Config exposing (Config)
import Http


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
        



