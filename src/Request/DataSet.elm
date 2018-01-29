module Request.DataSet exposing (get, getRetrieveData)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.DataSet as DataSet exposing (DataSet, DataSetData, DataSetList, DataSetName, dataSetNameToString)
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


get : Config -> Int -> Http.Request DataSetList
get { baseUrl, apiKey } page =
    let
        expect =
            DataSet.decodeDataSetList
                |> Http.expectJson

        params =
            pageParams page Config.pageSize
    in
    (baseUrl ++ "/data")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withQueryParams params
        |> withAuthorization (Just apiKey)
        |> HttpBuilder.toRequest


getRetrieveData : Config -> DataSetName -> Http.Request DataSetData
getRetrieveData { baseUrl, apiKey } name =
    let
        expect =
            DataSet.decodeDataSetData
                |> Http.expectJson

        params =
            pageParams 0 Config.pageSize
    in
    (baseUrl ++ "/data/" ++ dataSetNameToString name)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withQueryParams params
        |> withAuthorization (Just apiKey)
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]
