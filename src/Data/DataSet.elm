module Data.DataSet
    exposing
        ( ColumnMetadata
        , ColumnStats
        , ColumnStatsDict
        , Data
        , DataSet
        , DataSetData
        , DataSetList
        , DataSetName
        , DataSetStats
        , dataSetNameDecoder
        , dataSetNameParser
        , dataSetNameToString
        , decodeDataSetData
        , decodeDataSetList
        , decodeDataSetStats
        )

import Combine exposing ((<$>))
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, dict, float, int, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


{-| Returned from /data/{dataSetName}
Details of the dataset, a List of data, and paging information for the data.
-}
type alias DataSetData =
    { dataSetName : DataSetName
    , dataSetSize : Int
    , isTimeSeries : Bool
    , columns : List ColumnMetadata
    , data : Data
    , pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    }


type alias DataSetList =
    { items : List DataSet
    , pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    }


type alias DataSet =
    { dataSetName : DataSetName
    , dataSetSize : Int
    , isTimeSeries : Bool
    }


type alias ColumnMetadata =
    { dataType : String
    , role : String
    , imputation : String
    , aggregation : String
    , name : String
    }


type alias ColumnStats =
    { errors : Int
    , max : Float
    , mean : Float
    , median : Float
    , min : Float
    , non_numeric : Int
    , row_count : Int
    , stddev : Float
    , variance : Float
    }


type alias ColumnStatsDict =
    Dict String ColumnStats


type alias DataSetStats =
    { dataSetName : String
    , columns : ColumnStatsDict
    }


type alias Data =
    List (Dict String String)


type DataSetName
    = DataSetName String


dataSetNameToString : DataSetName -> String
dataSetNameToString (DataSetName name) =
    name


dataSetNameParser : Combine.Parser s DataSetName
dataSetNameParser =
    DataSetName <$> Combine.regex "[^/]+"


dataSetNameDecoder : Decode.Decoder DataSetName
dataSetNameDecoder =
    string
        |> Decode.map DataSetName


decodeDataSetList : Decode.Decoder DataSetList
decodeDataSetList =
    decode DataSetList
        |> required "items" (Decode.list decodeDataSet)
        |> required "pageNumber" Decode.int
        |> required "totalPages" Decode.int
        |> required "pageSize" Decode.int
        |> required "totalCount" Decode.int


decodeDataSet : Decode.Decoder DataSet
decodeDataSet =
    decode DataSet
        |> required "dataSetName" dataSetNameDecoder
        |> optional "dataSetSize" Decode.int 0
        |> required "isTimeSeries" Decode.bool


decodeDataSetData : Decode.Decoder DataSetData
decodeDataSetData =
    decode DataSetData
        |> required "dataSetName" dataSetNameDecoder
        |> optional "dataSetSize" Decode.int 0
        |> required "isTimeSeries" Decode.bool
        |> required "columns" decodeDataSetColumnsMetadata
        |> required "data" decodeData
        |> required "pageNumber" Decode.int
        |> required "totalPages" Decode.int
        |> required "pageSize" Decode.int
        |> required "totalCount" Decode.int


decodeDataSetColumnsMetadata : Decode.Decoder (List ColumnMetadata)
decodeDataSetColumnsMetadata =
    decode ColumnMetadata
        |> required "dataType" Decode.string
        |> required "role" Decode.string
        |> optional "imputation" Decode.string ""
        |> optional "aggregation" Decode.string ""
        |> Decode.keyValuePairs
        |> Decode.map (\a -> List.map (uncurry (|>)) a)


decodeData : Decoder Data
decodeData =
    list <|
        dict string


decodeDataSetStats : Decoder DataSetStats
decodeDataSetStats =
    decode DataSetStats
        |> required "dataSetName" string
        |> required "columns" decodeColumnStatsDict


decodeColumnStatsDict : Decoder ColumnStatsDict
decodeColumnStatsDict =
    dict decodeColumnStats


decodeColumnStats : Decoder ColumnStats
decodeColumnStats =
    decode ColumnStats
        |> required "errors" int
        |> required "max" float
        |> required "mean" float
        |> required "median" float
        |> required "min" float
        |> required "non_numeric" int
        |> required "row_count" int
        |> required "stddev" float
        |> required "variance" float
