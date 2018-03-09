module Request.Subscription exposing (list)

import Data.Config as Config exposing (Config)
import Data.Subscription exposing (..)
import Http
import HttpBuilder exposing (get, toRequest, withCredentials, withExpectJson)


list : Config -> Http.Request (List Subscription)
list { subscriptionUrl } =
    subscriptionUrl
        |> get
        |> withExpectJson decodeSubscriptionList
        |> withCredentials
        |> toRequest
