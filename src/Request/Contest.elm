module Request.Contest exposing (..)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.Contest exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)



get : Config -> String -> Http.Request Contest
get { baseUrl, token } sessionId =
    let
        expect =
            decodeContest
                |> Http.expectJson
    in
    (baseUrl ++ "/contest/" ++ sessionId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization token
        |> HttpBuilder.toRequest
