module Data.Algorithm exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, field, float, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)

type alias Algorithm = 
    { key : Maybe String
    , name : String
    , description : String
    }

decodeAlgorithm : Decoder Algorithm
decodeAlgorithm = 
    decode Algorithm
        |> optional "key" (Decode.map Just string) Nothing
        |> required "name" Decode.string
        |> required "description" Decode.string