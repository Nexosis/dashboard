module Data.Columns exposing (dataTypeToString, enumDataType, enumRole)

import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode as Encode


enumDataType : List DataType
enumDataType =
    [ Measure
    , Numeric
    , String
    , Logical
    , Date
    , Text
    ]


enumRole : List Role
enumRole =
    [ None
    , Timestamp
    , Target
    , Feature
    ]


dataTypeToString : DataType -> String
dataTypeToString dataType =
    if dataType == Measure then
        "numericMeasure"
    else
        toString dataType


defaultColumnMetadata : ColumnMetadata
defaultColumnMetadata =
    ColumnMetadata Measure None Impute.Mean Aggregate.Mean ""
