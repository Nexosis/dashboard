module Data.ConfusionMatrix exposing (ConfusionMatrix, decodeConfusionMatrix)

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder, andThen, array, dict, fail, field, float, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias ConfusionMatrix =
    { classes : Array String
    , confusionMatrix : Array (Array Int)
    }


decodeConfusionMatrix : Decode.Decoder ConfusionMatrix
decodeConfusionMatrix =
    decode ConfusionMatrix
        |> required "classes" (array string)
        |> required "confusionMatrix" (array (array int))
