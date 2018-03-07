module Request.Subscription exposing (list)

import Data.Config as Config exposing (Config)
import Data.Subscription exposing (..)
import Http
import HttpBuilder exposing (get, toRequest, withCredentials, withExpect)


list : Config -> Http.Request (List Subscription)
list { subscriptionUrl } =
    let
        expect =
            decodeSubscriptionList
                |> Http.expectJson
    in
    subscriptionUrl
        |> get
        |> withExpect expect
        |> withCredentials
        |> toRequest
