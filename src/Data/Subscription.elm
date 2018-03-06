module Data.Subscription exposing (..)

import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias Subscription =
    { id : String
    , name : String
    }


type alias SubscriptionKey =
    { id : String
    , name : String
    , key : String
    }


decodeSubscriptionList : Decoder (List Subscription)
decodeSubscriptionList =
    list decodeSubscription


decodeSubscription : Decoder Subscription
decodeSubscription =
    decode Subscription
        |> required "id" string
        |> required "name" string


decodeSubscriptionKey : Decoder SubscriptionKey
decodeSubscriptionKey =
    decode SubscriptionKey
        |> required "id" string
        |> required "name" string
        |> required "key" string
