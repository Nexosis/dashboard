module Data.Subscription exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, dict, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias Subscription =
    { id : String
    , name : String
    , key : Maybe String
    }


decodeSubscriptionList : Decoder (List Subscription)
decodeSubscriptionList =
    list decodeSubscription


decodeSubscription : Decoder Subscription
decodeSubscription =
    decode Subscription
        |> required "Id" string
        |> required "Name" string
        |> optional "key" (Decode.map Just string) Nothing
