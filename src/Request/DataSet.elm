module Request.DataSet exposing (get)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.DataSet as DataSet exposing (DataSet, DataSetColumnsDate, DataSetColumnsQuantity, DataSetList)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode
import Json.Decode.Pipeline
import Json.Encode as Encode
import Util exposing ((=>))


encodeDataSetList : DataSetList -> Encode.Value
encodeDataSetList record =
    Encode.object
        [ ( "items", Encode.list <| List.map encodeDataSet <| record.items )
        , ( "pageNumber", Encode.int <| record.pageNumber )
        , ( "totalPages", Encode.int <| record.totalPages )
        , ( "pageSize", Encode.int <| record.pageSize )
        , ( "totalCount", Encode.int <| record.totalCount )
        ]


encodeDataSet : DataSet -> Encode.Value
encodeDataSet record =
    Encode.object
        [ ( "dataSetName", Encode.string <| record.dataSetName )
        , ( "dataSetSize", Encode.int <| record.dataSetSize )
        , ( "isTimeSeries", Encode.bool <| record.isTimeSeries )
        ]


encodeDataSetColumnsDate : DataSetColumnsDate -> Encode.Value
encodeDataSetColumnsDate record =
    Encode.object
        [ ( "dataType", Encode.string <| record.dataType )
        , ( "role", Encode.string <| record.role )
        ]


encodeDataSetColumnsQuantity : DataSetColumnsQuantity -> Encode.Value
encodeDataSetColumnsQuantity record =
    Encode.object
        [ ( "dataType", Encode.string <| record.dataType )
        , ( "role", Encode.string <| record.role )
        , ( "imputation", Encode.string <| record.imputation )
        , ( "aggregation", Encode.string <| record.aggregation )
        ]


get : Config -> Int -> Http.Request DataSetList
get { baseUrl, apiKey } page =
    let
        expect =
            DataSet.decodeDataSetList
                |> Http.expectJson

        url =
            baseUrl

        params =
            [ ( "page", page |> toString )
            , ( "pageSize", Config.pageSize |> toString )
            ]
    in
    (baseUrl ++ "/data")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withQueryParams params
        |> withAuthorization (Just apiKey)
        |> HttpBuilder.toRequest
