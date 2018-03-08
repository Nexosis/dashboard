module Data.Subscription exposing (..)

import Json.Decode as Decode exposing (Decoder, dict, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias Subscription =
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
        |> required "Id" string
        |> required "Name" string
        |> required "Key" string
