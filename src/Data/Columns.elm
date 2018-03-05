module Data.Columns exposing (ColumnMetadata, DataType(..), Role(..), decodeColumnMetadata, enumDataType, enumRole, stringToDataType, stringToRole)

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


enumDataType : List DataType
enumDataType =
    [ NumericMeasure
    , Numeric
    , String
    , Logical
    , Date
    , Text
    ]


type Role
    = None
    | Timestamp
    | Target
    | Feature
    | Key


enumRole : List Role
enumRole =
    [ None
    , Timestamp
    , Target
    , Feature
    , Key
    ]


decodeColumnMetadata : Decoder (List ColumnMetadata)
decodeColumnMetadata =
    decode ColumnMetadata
        |> optional "dataType" decodeDataType String
        |> optional "role" decodeRole None
        |> optional "imputation" decodeImputation Impute.Mean
        |> optional "aggregation" decodeAggregation Aggregate.Mean
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


stringToDataType : String -> Result String DataType
stringToDataType input =
    case String.toLower input of
        "numericmeasure" ->
            Ok NumericMeasure

        "string" ->
            Ok String

        "numeric" ->
            Ok Numeric

        "logical" ->
            Ok Logical

        "data" ->
            Ok Date

        "text" ->
            Ok Text

        _ ->
            Err "unknown datatype input provided"


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


stringToRole : String -> Result String Role
stringToRole input =
    case String.toLower input of
        "none" ->
            Ok None

        "timestamp" ->
            Ok Timestamp

        "target" ->
            Ok Target

        "feature" ->
            Ok Feature

        "key" ->
            Ok Key

        _ ->
            Err "Unknown role type value provided"


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
