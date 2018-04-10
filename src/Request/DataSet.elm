module Request.DataSet exposing (MetadataUpdateRequest, PutUploadRequest, UploadData(..), createDataSetWithKey, delete, encodeKeyColumnMetadata, get, getDataByDateRange, getRetrieveDetail, getStats, getStatsForColumn, put, updateMetadata)

import Csv
import Data.Columns exposing (ColumnMetadata, DataType, dataTypeToString, encodeColumnMetadataList)
import Data.Config as Config exposing (Config, withAppHeader)
import Data.DataFormat as DataFormat exposing (DataFormat(..), dataFormatToContentType)
import Data.DataSet as DataSet exposing (DataSet, DataSetData, DataSetList, DataSetName, DataSetStats, dataSetNameToString)
import Data.File as File
import Http
import HttpBuilder exposing (RequestBuilder, withExpectJson)
import Json.Decode as Decode
import Json.Encode as Encode
import Nexosis exposing (ClientConfig, withAuthorization)
import Request.Sorting exposing (SortDirection(..), SortParameters, sortParams)
import Set


get : Config -> Int -> Int -> SortParameters -> Http.Request DataSetList
get config page pageSize sorting =
    let
        params =
            pageParams page pageSize
                ++ sortParams sorting
    in
    (config.clientConfig.url ++ "/data")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetList
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


getRetrieveDetail : Config -> DataSetName -> Int -> Int -> Http.Request DataSetData
getRetrieveDetail config name pgNum pgSize =
    let
        params =
            pageParams pgNum pgSize
    in
    (config.clientConfig.url ++ "/data/" ++ uriEncodeDataSetName name)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetData
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


getDataByDateRange : Config -> DataSetName -> Maybe ( String, String ) -> List String -> Http.Request DataSetData
getDataByDateRange config name dateRange include =
    let
        params =
            pageParams 0 1000
                ++ dateParams dateRange
                ++ includeParams include
                ++ [ ( "formatDates", "true" ) ]
    in
    (config.clientConfig.url ++ "/data/" ++ uriEncodeDataSetName name)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetData
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


getStats : Config -> DataSetName -> Http.Request DataSetStats
getStats config name =
    (config.clientConfig.url ++ "/data/" ++ uriEncodeDataSetName name ++ "/stats")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetStats
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


getStatsForColumn : Config -> DataSetName -> String -> DataType -> Http.Request DataSetStats
getStatsForColumn config dataSetName columnName columnType =
    (config.clientConfig.url ++ "/data/" ++ uriEncodeDataSetName dataSetName ++ "/stats/" ++ Http.encodeUri columnName)
        |> HttpBuilder.get
        |> HttpBuilder.withQueryParams [ ( "dataType", dataTypeToString columnType ) ]
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetStats
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


delete : Config -> DataSetName -> Set.Set String -> Http.Request ()
delete config name cascadeOptions =
    let
        cascadeList =
            Set.toList cascadeOptions
                |> List.map (\c -> ( "cascade", c ))
    in
    (config.clientConfig.url ++ "/data/" ++ uriEncodeDataSetName name)
        |> HttpBuilder.delete
        |> HttpBuilder.withQueryParams cascadeList
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


type alias PutUploadRequest =
    { name : String
    , data : UploadData
    }


type UploadData
    = Json File.JsonData
    | Csv Csv.Csv


put : Config -> PutUploadRequest -> List (Http.Request ())
put config request =
    batch request <| putInternal config


batch : PutUploadRequest -> (( String, String, String ) -> a) -> List a
batch request put =
    case request.data of
        Json json ->
            let
                jsonDataToString data =
                    Encode.encode 0 (File.jsonDataEncoder data)

                upload data =
                    put ( request.name, jsonDataToString data, dataFormatToContentType DataFormat.Json )
            in
            File.batchJsonData 2000 upload <| json

        Csv csv ->
            let
                csvDataToString data =
                    let
                        sep =
                            String.join ","

                        header =
                            sep data.headers

                        line =
                            String.join "\x0D\n"
                    in
                    line <| [ header ] ++ List.map sep data.records

                upload data =
                    put ( request.name, csvDataToString data, dataFormatToContentType DataFormat.Csv )
            in
            File.batchCsvData 3000 upload <| csv


putInternal : Config -> ( String, String, String ) -> Http.Request ()
putInternal config ( name, content, contentType ) =
    (config.clientConfig.url ++ "/data/" ++ Http.encodeUri name)
        |> HttpBuilder.put
        |> HttpBuilder.withBody (Http.stringBody contentType content)
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]


dateParams : Maybe ( String, String ) -> List ( String, String )
dateParams dateRange =
    case dateRange of
        Just dates ->
            [ ( "startDate", Tuple.first dates ), ( "endDate", Tuple.second dates ) ]

        Nothing ->
            []


includeParams : List String -> List ( String, String )
includeParams includes =
    includes |> List.map (\value -> ( "include", value ))


updateMetadata : Config -> MetadataUpdateRequest -> Http.Request ()
updateMetadata config request =
    (config.clientConfig.url ++ "/data/" ++ uriEncodeDataSetName request.dataSetName)
        |> HttpBuilder.put
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.withJsonBody (encodeMetadataPutDataRequest request)
        |> HttpBuilder.toRequest


type alias MetadataUpdateRequest =
    { dataSetName : DataSetName
    , columns : List ColumnMetadata
    }


encodeMetadataPutDataRequest : MetadataUpdateRequest -> Encode.Value
encodeMetadataPutDataRequest request =
    Encode.object
        [ ( "dataSetName", Encode.string <| dataSetNameToString request.dataSetName )
        , ( "columns", encodeColumnMetadataList <| request.columns )
        ]


uriEncodeDataSetName : DataSetName -> String
uriEncodeDataSetName name =
    Http.encodeUri <| dataSetNameToString name


createDataSetWithKey : Config -> String -> String -> Http.Request ()
createDataSetWithKey config dataSetName keyName =
    let
        keyBody =
            Encode.object [ ( "columns", encodeKeyColumnMetadata keyName ) ]
    in
    (config.clientConfig.url ++ "/data/" ++ Http.encodeUri dataSetName)
        |> HttpBuilder.put
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.withJsonBody keyBody
        |> HttpBuilder.toRequest


encodeKeyColumnMetadata : String -> Encode.Value
encodeKeyColumnMetadata key =
    Encode.object <|
        [ ( key
          , Encode.object [ ( "role", Encode.string "key" ) ]
          )
        ]
