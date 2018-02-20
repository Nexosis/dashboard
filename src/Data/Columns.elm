module Data.Columns exposing (ColumnMetadata, DataType(..), Role(..), decodeColumnMetadata)

import Data.AggregationStrategy as Aggregate exposing (AggregationStrategy)
import Data.ImputationStrategy as Impute exposing (ImputationStrategy)
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias ColumnMetadata =
    { dataType : DataType
    , role : Role
    , imputation : ImputationStrategy
    , aggregation : AggregationStrategy
    , name : String
    }


type DataType
    = NumericMeasure
    | String
    | Numeric
    | Logical
    | Date
    | Text


type Role
    = None
    | Timestamp
    | Target
    | Feature
    | Key


decodeColumnMetadata : Decoder (List ColumnMetadata)
decodeColumnMetadata =
    decode ColumnMetadata
        |> optional "dataType" decodeDataType String
        |> optional "role" decodeRole None
        |> optional "imputation" decodeImputation Impute.Zeroes
        |> optional "aggregation" decodeAggregation Aggregate.Sum
        |> Decode.keyValuePairs
        |> Decode.map (\a -> List.map (uncurry (|>)) a)


decodeDataType : Decoder DataType
decodeDataType =
    string
        |> andThen
            (\columnType ->
                case String.toLower columnType of
                    "numericmeasure" ->
                        succeed NumericMeasure

                    "string" ->
                        succeed String

                    "numeric" ->
                        succeed Numeric

                    "logical" ->
                        succeed Logical

                    "date" ->
                        succeed Date

                    "text" ->
                        succeed Text

                    unknown ->
                        fail <| "Unknown columnType: " ++ unknown
            )


decodeRole : Decoder Role
decodeRole =
    string
        |> andThen
            (\role ->
                case String.toLower role of
                    "none" ->
                        succeed None

                    "timestamp" ->
                        succeed Timestamp

                    "target" ->
                        succeed Target

                    "feature" ->
                        succeed Feature

                    "key" ->
                        succeed Key

                    unknown ->
                        fail <| "Unknown column role: " ++ unknown
            )


decodeImputation : Decoder ImputationStrategy
decodeImputation =
    string
        |> andThen
            (\imputation ->
                case String.toLower imputation of
                    "zeroes" ->
                        succeed Impute.Zeroes

                    "mean" ->
                        succeed Impute.Mean

                    "median" ->
                        succeed Impute.Median

                    "mode" ->
                        succeed Impute.Mode

                    "min" ->
                        succeed Impute.Min

                    "max" ->
                        succeed Impute.Max

                    unknown ->
                        fail <| "Unknown imputation strategy: " ++ unknown
            )


decodeAggregation : Decoder AggregationStrategy
decodeAggregation =
    string
        |> andThen
            (\aggregation ->
                case String.toLower aggregation of
                    "sum" ->
                        succeed Aggregate.Sum

                    "mean" ->
                        succeed Aggregate.Mean

                    "median" ->
                        succeed Aggregate.Median

                    "mode" ->
                        succeed Aggregate.Mode

                    "min" ->
                        succeed Aggregate.Min

                    "max" ->
                        succeed Aggregate.Max

                    unknown ->
                        fail <| "Unknown aggregation strategy: " ++ unknown
            )
