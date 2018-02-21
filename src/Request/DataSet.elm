module Request.DataSet exposing (delete, get, getRetrieveDetail, getStats)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.DataSet as DataSet exposing (DataSet, DataSetData, DataSetList, DataSetName, DataSetStats, dataSetNameToString)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Set


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


getStats : Config -> DataSetName -> Http.Request DataSetStats
getStats { baseUrl, token } name =
    let
        expect =
            DataSet.decodeDataSetStats
                |> Http.expectJson
    in
    (baseUrl ++ "/data/" ++ dataSetNameToString name ++ "/stats")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization token
        |> HttpBuilder.toRequest


delete : Config -> DataSetName -> Set.Set String -> Http.Request ()
delete { baseUrl, token } name cascadeOptions =
    let
        cascadeList =
            Set.toList cascadeOptions
                |> List.map (\c -> ( "cascade", c ))
    in
    (baseUrl ++ "/data/" ++ dataSetNameToString name)
        |> HttpBuilder.delete
        |> HttpBuilder.withQueryParams cascadeList
        |> withAuthorization token
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]
