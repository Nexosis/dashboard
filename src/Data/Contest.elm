module Data.Contest exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, field, float, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Nexosis.Decoders.Algorithm exposing (decodeAlgorithm)
import Nexosis.Types.Algorithm exposing (Algorithm)


type alias Contestant =
    { id : String
    , algorithm : Algorithm
    , dataSourceProperties : List String
    , metrics : Dict String Float
    }


type alias Contest =
    { championMetric : String
    , champion : Contestant
    , contestants : List Contestant
    }


decodeContestant : Decode.Decoder Contestant
decodeContestant =
    decode Contestant
        |> required "id" Decode.string
        |> required "algorithm" decodeAlgorithm
        |> required "dataSourceProperties" (Decode.list Decode.string)
        |> required "metrics" (Decode.dict Decode.float)


decodeContest : Decoder Contest
decodeContest =
    decode Contest
        |> required "championMetric" Decode.string
        |> required "champion" decodeContestant
        |> required "contestants" (Decode.list decodeContestant)
