module Request.DataSet exposing (MetadataUpdateRequest, PutUploadRequest, delete, get, getDataByDateRange, getRetrieveDetail, getStats, put, updateMetadata)

import Data.Columns exposing (ColumnMetadata, encodeColumnMetadataList)
import Data.Config as Config exposing (Config, withAuthorization)
import Data.DataSet as DataSet exposing (DataSet, DataSetData, DataSetList, DataSetName, DataSetStats, dataSetNameToString)
import Http
import HttpBuilder exposing (RequestBuilder, withExpectJson)
import Json.Encode as Encode
import Set


get : Config -> Int -> Int -> Http.Request DataSetList
get config page pageSize =
    let
        params =
            pageParams page pageSize
    in
    (config.baseUrl ++ "/data")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetList
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest


getRetrieveDetail : Config -> DataSetName -> Http.Request DataSetData
getRetrieveDetail config name =
    let
        params =
            pageParams 0 1
    in
    (config.baseUrl ++ "/data/" ++ uriEncodeDataSetName name)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetData
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
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
    (config.baseUrl ++ "/data/" ++ uriEncodeDataSetName name)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetData
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest


getStats : Config -> DataSetName -> Http.Request DataSetStats
getStats config name =
    (config.baseUrl ++ "/data/" ++ uriEncodeDataSetName name ++ "/stats")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson DataSet.decodeDataSetStats
        |> withAuthorization config
        |> HttpBuilder.toRequest


delete : Config -> DataSetName -> Set.Set String -> Http.Request ()
delete config name cascadeOptions =
    let
        cascadeList =
            Set.toList cascadeOptions
                |> List.map (\c -> ( "cascade", c ))
    in
    (config.baseUrl ++ "/data/" ++ uriEncodeDataSetName name)
        |> HttpBuilder.delete
        |> HttpBuilder.withQueryParams cascadeList
        |> withAuthorization config
        |> HttpBuilder.toRequest


type alias PutUploadRequest =
    { name : String
    , content : String
    , contentType : String
    }


put : Config -> PutUploadRequest -> Http.Request ()
put config { name, content, contentType } =
    (config.baseUrl ++ "/data/" ++ Http.encodeUri name)
        |> HttpBuilder.put
        |> HttpBuilder.withBody (Http.stringBody contentType content)
        |> withAuthorization config
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
    (config.baseUrl ++ "/data/" ++ uriEncodeDataSetName request.dataSetName)
        |> HttpBuilder.put
        |> withAuthorization config
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
