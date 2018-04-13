module Data.Columns exposing (defaultColumnMetadata, enumDataType, enumRole)

import Nexosis.Types.AggregationStrategy as Aggregate
import Nexosis.Types.Columns exposing (ColumnMetadata, DataType(..), Role(..))
import Nexosis.Types.ImputationStrategy as Impute


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


defaultColumnMetadata : ColumnMetadata
defaultColumnMetadata =
    ColumnMetadata Measure None Impute.Mean Aggregate.Mean ""
