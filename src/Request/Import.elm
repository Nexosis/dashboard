module Request.Import exposing (PostS3Request, PostUrlRequest, get, postS3, postUrl)

import Data.Config exposing (Config, withAuthorization)
import Data.Import exposing (ImportDetail, decodeImportDetail)
import Http
import HttpBuilder exposing (withExpectJson)
import Json.Encode exposing (Value, encode)


type alias PostUrlRequest =
    { dataSetName : String
    , url : String
    }


type alias PostS3Request =
    { dataSetName : String
    , bucket : String
    , path : String
    , region : Maybe String
    , accessKeyId : Maybe String
    , secretAccessKey : Maybe String
    }


postUrl : Config -> PostUrlRequest -> Http.Request ImportDetail
postUrl config { dataSetName, url } =
    (config.baseUrl ++ "/imports/url")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody "application/json" <| encode 0 (encodeImportUrl dataSetName url))
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


get : Config -> String -> Http.Request ImportDetail
get config importId =
    (config.baseUrl ++ "/imports/" ++ importId)
        |> HttpBuilder.get
        |> withAuthorization config
        |> withExpectJson decodeImportDetail
        |> HttpBuilder.toRequest


encodeImportUrl : String -> String -> Json.Encode.Value
encodeImportUrl dataSetName url =
    Json.Encode.object
        [ ( "dataSetName", Json.Encode.string <| dataSetName )
        , ( "url", Json.Encode.string <| url )
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
        , ( "accessKeyId", encodeMaybe <| request.accessKeyId )
        , ( "secretAccessKey", encodeMaybe <| request.secretAccessKey )
        ]
