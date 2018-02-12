module Data.DataSet
    exposing
        ( Data
        , DataSet
        , DataSetColumnsMetadata
        , DataSetData
        , DataSetList
        , DataSetName
        , dataSetNameDecoder
        , dataSetNameParser
        , dataSetNameToString
        , decodeDataSetData
        , decodeDataSetList
        )

import Combine exposing ((<$), (<$>))
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline


{-| Returned from /data/{dataSetName}
Details of the dataset, a List of data, and paging information for the data.
-}
type alias DataSetData =
    { dataSetName : DataSetName
    , dataSetSize : Int
    , isTimeSeries : Bool
    , columns : List DataSetColumnsMetadata
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


type alias DataSetColumnsMetadata =
    { dataType : String
    , role : String
    , imputation : String
    , aggregation : String
    , name : String
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
    Decode.string
        |> Decode.map DataSetName


decodeDataSetList : Decode.Decoder DataSetList
decodeDataSetList =
    Json.Decode.Pipeline.decode DataSetList
        |> Json.Decode.Pipeline.required "items" (Decode.list decodeDataSet)
        |> Json.Decode.Pipeline.required "pageNumber" Decode.int
        |> Json.Decode.Pipeline.required "totalPages" Decode.int
        |> Json.Decode.Pipeline.required "pageSize" Decode.int
        |> Json.Decode.Pipeline.required "totalCount" Decode.int


decodeDataSet : Decode.Decoder DataSet
decodeDataSet =
    Json.Decode.Pipeline.decode DataSet
        |> Json.Decode.Pipeline.required "dataSetName" dataSetNameDecoder
        |> Json.Decode.Pipeline.optional "dataSetSize" Decode.int 0
        |> Json.Decode.Pipeline.required "isTimeSeries" Decode.bool


decodeDataSetData : Decode.Decoder DataSetData
decodeDataSetData =
    Json.Decode.Pipeline.decode DataSetData
        |> Json.Decode.Pipeline.required "dataSetName" dataSetNameDecoder
        |> Json.Decode.Pipeline.optional "dataSetSize" Decode.int 0
        |> Json.Decode.Pipeline.required "isTimeSeries" Decode.bool
        |> Json.Decode.Pipeline.required "columns" decodeDataSetColumnsMetadata
        |> Json.Decode.Pipeline.required "data" decodeData
        |> Json.Decode.Pipeline.required "pageNumber" Decode.int
        |> Json.Decode.Pipeline.required "totalPages" Decode.int
        |> Json.Decode.Pipeline.required "pageSize" Decode.int
        |> Json.Decode.Pipeline.required "totalCount" Decode.int


decodeDataSetColumnsMetadata : Decode.Decoder (List DataSetColumnsMetadata)
decodeDataSetColumnsMetadata =
    Json.Decode.Pipeline.decode DataSetColumnsMetadata
        |> Json.Decode.Pipeline.required "dataType" Decode.string
        |> Json.Decode.Pipeline.required "role" Decode.string
        |> Json.Decode.Pipeline.optional "imputation" Decode.string ""
        |> Json.Decode.Pipeline.optional "aggregation" Decode.string ""
        |> Decode.keyValuePairs
        |> Decode.map (\a -> List.map (uncurry (|>)) a)


decodeData : Decode.Decoder Data
decodeData =
    Decode.list <|
        Decode.dict Decode.string
