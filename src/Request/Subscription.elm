module Request.Subscription exposing (getKey, list)

import Data.Config as Config exposing (Config)
import Data.Subscription exposing (..)
import Dict exposing (Dict)
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


getKey : Config -> String -> Http.Request Subscription
getKey { subscriptionUrl } id =
    let
        expect =
            decodeSubscription
                |> Http.expectJson
    in
    subscriptionUrl
        ++ "/"
        ++ id
        |> get
        |> withExpect expect
        |> withCredentials
        |> toRequest
