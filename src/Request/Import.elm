module Request.Import exposing (PostAzureRequest, PostS3Request, PostUrlRequest, get, postAzure, postS3, postUrl)

import Data.Config exposing (Config, withAuthorization)
import Data.Import exposing (ImportDetail, decodeImportDetail)
import Http
import HttpBuilder exposing (withExpectJson)
import Json.Encode exposing (Value, encode)
import Request.DataSet exposing (encodeKeyColumnMetadata)


type alias PostUrlRequest =
    { dataSetName : String
    , url : String
    , key : Maybe String
    }


type alias PostS3Request =
    { dataSetName : String
    , bucket : String
    , path : String
    , region : Maybe String
    , accessKeyId : Maybe String
    , secretAccessKey : Maybe String
    }


type alias PostAzureRequest =
    { dataSetName : String
    , connectionString : String
    , container : String
    , blob : String
    }


postUrl : Config -> PostUrlRequest -> Http.Request ImportDetail
postUrl config { dataSetName, url, key } =
    (config.baseUrl ++ "/imports/url")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody "application/json" <| encode 0 (encodeImportUrl dataSetName url key))
        |> withAuthorization config
        |> withExpectJson decodeImportDetail
        |> HttpBuilder.toRequest


postS3 : Config -> PostS3Request -> Http.Request ImportDetail
postS3 config request =
    (config.baseUrl ++ "/imports/s3")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody "application/json" <| encode 0 (encodeImportS3 request))
        |> withAuthorization config
        |> withExpectJson decodeImportDetail
        |> HttpBuilder.toRequest


postAzure : Config -> PostAzureRequest -> Http.Request ImportDetail
postAzure config request =
    (config.baseUrl ++ "/imports/azure")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody "application/json" <| encode 0 (encodeImportAzure request))
        |> withAuthorization config
        |> withExpectJson decodeImportDetail
        |> HttpBuilder.toRequest


get : Config -> String -> Http.Request ImportDetail
get config importId =
    (config.baseUrl ++ "/imports/" ++ importId)
        |> HttpBuilder.get
        |> withAuthorization config
        |> withExpectJson decodeImportDetail
        |> HttpBuilder.toRequest


encodeImportUrl : String -> String -> Maybe String -> Json.Encode.Value
encodeImportUrl dataSetName url key =
    let
        keyEncoder =
            key
                |> Maybe.map encodeKeyColumnMetadata
                |> Maybe.withDefault Json.Encode.null
    in
    Json.Encode.object
        [ ( "dataSetName", Json.Encode.string <| dataSetName )
        , ( "url", Json.Encode.string <| url )
        , ( "columns", keyEncoder )
        ]


encodeImportS3 : PostS3Request -> Json.Encode.Value
encodeImportS3 request =
    let
        encodeMaybe maybe =
            case maybe of
                Nothing ->
                    Json.Encode.null

                Just thing ->
                    Json.Encode.string thing
    in
    Json.Encode.object
        [ ( "dataSetName", Json.Encode.string <| request.dataSetName )
        , ( "path", Json.Encode.string <| request.path )
        , ( "bucket", Json.Encode.string <| request.bucket )
        , ( "region", encodeMaybe <| request.region )
        , ( "accessKeyId", encodeMaybe <| request.accessKeyId )
        , ( "secretAccessKey", encodeMaybe <| request.secretAccessKey )
        ]


encodeImportAzure : PostAzureRequest -> Json.Encode.Value
encodeImportAzure request =
    Json.Encode.object
        [ ( "dataSetName", Json.Encode.string <| request.dataSetName )
        , ( "connectionString", Json.Encode.string <| request.connectionString )
        , ( "container", Json.Encode.string <| request.container )
        , ( "blob", Json.Encode.string <| request.blob )
        ]
