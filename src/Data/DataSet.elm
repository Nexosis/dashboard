module Data.DataSet
    exposing
        ( ColumnStats
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
        , toDataSetName
        )

import Combine exposing ((<$>))
import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Data.DisplayDate exposing (dateDecoder, toShortDateTimeString)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, dict, fail, float, int, list, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Time.ZonedDateTime exposing (ZonedDateTime)
import Util exposing (formatFloatToString)


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
    , dateCreated : ZonedDateTime
    , lastModified : ZonedDateTime
    , rowCount : Int
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
    , dateCreated : ZonedDateTime
    , lastModified : ZonedDateTime
    , rowCount : Int
    , columnCount : Int
    }


type alias ColumnStats =
    { distinctCount : Int
    , errorCount : Int
    , lastCalculated : ZonedDateTime
    , max : String
    , mean : Float
    , median : Float
    , min : String
    , missingCount : Int
    , mode : String
    , stddev : Float
    , suggestedType : String
    , totalCount : Int
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


toDataSetName : String -> DataSetName
toDataSetName input =
    DataSetName input


dataSetNameToString : DataSetName -> String
dataSetNameToString (DataSetName name) =
    name


dataSetNameParser : Combine.Parser s DataSetName
dataSetNameParser =
    DataSetName <$> Combine.regex "[^/]+"


dataSetNameDecoder : Decoder DataSetName
dataSetNameDecoder =
    string
        |> Decode.andThen (\n -> succeed (Maybe.withDefault "" (Http.decodeUri n)))
        |> Decode.map DataSetName


decodeDataSetList : Decoder DataSetList
decodeDataSetList =
    decode DataSetList
        |> required "items" (Decode.list decodeDataSet)
        |> required "pageNumber" Decode.int
        |> required "totalPages" Decode.int
        |> required "pageSize" Decode.int
        |> required "totalCount" Decode.int


decodeDataSet : Decoder DataSet
decodeDataSet =
    decode DataSet
        |> required "dataSetName" dataSetNameDecoder
        |> optional "dataSetSize" Decode.int 0
        |> required "isTimeSeries" Decode.bool
        |> required "dateCreated" dateDecoder
        |> required "lastModified" dateDecoder
        |> optional "rowCount" Decode.int 0
        |> required "columnCount" Decode.int


decodeDataSetData : Decoder DataSetData
decodeDataSetData =
    decode DataSetData
        |> required "dataSetName" dataSetNameDecoder
        |> optional "dataSetSize" Decode.int 0
        |> required "isTimeSeries" Decode.bool
        |> required "columns" decodeColumnMetadata
        |> required "data" decodeData
        |> required "pageNumber" Decode.int
        |> required "totalPages" Decode.int
        |> required "pageSize" Decode.int
        |> required "totalCount" Decode.int
        |> required "dateCreated" dateDecoder
        |> required "lastModified" dateDecoder
        |> optional "rowCount" Decode.int 0


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
        |> optional "distinctCount" int 0
        |> optional "errorCount" int 0
        |> required "lastCalculated" dateDecoder
        |> optional "max" variableDecoder ""
        |> optional "mean" float 0
        |> optional "median" float 0
        |> optional "min" variableDecoder ""
        |> optional "missingCount" int 0
        |> optional "mode" variableDecoder ""
        |> optional "stddev" float 0
        |> optional "suggestedType" string "numericMeasure"
        |> required "totalCount" int
        |> optional "variance" float 0


variableDecoder : Decoder String
variableDecoder =
    Decode.oneOf
        --Note that order matters.
        [ Decode.float |> Decode.andThen (\f -> succeed (formatFloatToString f))
        , Decode.bool |> Decode.andThen (\b -> succeed (toString b))
        , dateDecoder |> Decode.andThen (\d -> succeed (toShortDateTimeString d))
        , Decode.string
        ]
