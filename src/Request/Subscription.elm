module Request.Subscription exposing (getKey, list)

import Data.Config as Config exposing (Config)
import Data.Subscription exposing (..)
import Http
import HttpBuilder exposing (get, toRequest, withExpect)


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
        |> toRequest


getKey : Config -> String -> Http.Request SubscriptionKey
getKey { subscriptionUrl } id =
    let
        expect =
            decodeSubscriptionKey
                |> Http.expectJson
    in
    subscriptionUrl
        ++ "/"
        ++ id
        |> get
        |> withExpect expect
        |> toRequest
