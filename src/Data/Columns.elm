module Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)

import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias ColumnMetadata =
    { dataType : String
    , role : String
    , imputation : String
    , aggregation : String
    , name : String
    }


decodeColumnMetadata : Decode.Decoder (List ColumnMetadata)
decodeColumnMetadata =
    decode ColumnMetadata
        |> required "dataType" Decode.string
        |> required "role" Decode.string
        |> optional "imputation" Decode.string ""
        |> optional "aggregation" Decode.string ""
        |> Decode.keyValuePairs
        |> Decode.map (\a -> List.map (uncurry (|>)) a)
