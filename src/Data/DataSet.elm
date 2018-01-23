module Data.DataSet exposing (DataSet, DataSetList, DataSetColumnsDate, DataSetColumnsQuantity, decodeDataSetList)

import Json.Decode as Decode
import Json.Decode.Pipeline


type alias DataSetList =
    { items : List DataSet
    , pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    }


type alias DataSet =
    { dataSetName : String
    , dataSetSize : Int
    , isTimeSeries : Bool
    }


type alias DataSetColumnsDate =
    { dataType : String
    , role : String
    }


type alias DataSetColumnsQuantity =
    { dataType : String
    , role : String
    , imputation : String
    , aggregation : String
    }


type alias DataSetColumns =
    { date : DataSetColumnsDate
    , quantity : DataSetColumnsQuantity
    }


decodeDataSetList : Decode.Decoder DataSetList
decodeDataSetList =
    Json.Decode.Pipeline.decode DataSetList
        |> Json.Decode.Pipeline.required "items" (Decode.list decodeDataSet)
        |> Json.Decode.Pipeline.required "pageNumber" (Decode.int)
        |> Json.Decode.Pipeline.required "totalPages" (Decode.int)
        |> Json.Decode.Pipeline.required "pageSize" (Decode.int)
        |> Json.Decode.Pipeline.required "totalCount" (Decode.int)


decodeDataSet : Decode.Decoder DataSet
decodeDataSet =
    Json.Decode.Pipeline.decode DataSet
        |> Json.Decode.Pipeline.required "dataSetName" (Decode.string)
        |> Json.Decode.Pipeline.optional "dataSetSize" (Decode.int) 0
        |> Json.Decode.Pipeline.required "isTimeSeries" (Decode.bool)


decodeDataSetColumnsDate : Decode.Decoder DataSetColumnsDate
decodeDataSetColumnsDate =
    Json.Decode.Pipeline.decode DataSetColumnsDate
        |> Json.Decode.Pipeline.required "dataType" (Decode.string)
        |> Json.Decode.Pipeline.required "role" (Decode.string)


decodeDataSetColumnsQuantity : Decode.Decoder DataSetColumnsQuantity
decodeDataSetColumnsQuantity =
    Json.Decode.Pipeline.decode DataSetColumnsQuantity
        |> Json.Decode.Pipeline.required "dataType" (Decode.string)
        |> Json.Decode.Pipeline.required "role" (Decode.string)
        |> Json.Decode.Pipeline.required "imputation" (Decode.string)
        |> Json.Decode.Pipeline.required "aggregation" (Decode.string)
