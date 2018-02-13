module Request.DataSet exposing (get, getRetrieveDetail)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.DataSet as DataSet exposing (DataSet, DataSetData, DataSetList, DataSetName, dataSetNameToString)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)


get : Config -> Int -> Http.Request DataSetList
get { baseUrl, token } page =
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
        |> withAuthorization token
        |> HttpBuilder.toRequest


getRetrieveDetail : Config -> DataSetName -> Http.Request DataSetData
getRetrieveDetail { baseUrl, token } name =
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
        |> withAuthorization token
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]
