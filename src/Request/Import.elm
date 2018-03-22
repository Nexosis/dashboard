module Request.Import exposing (PostUrlRequest, get, postUrl)

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


postUrl : Config -> PostUrlRequest -> Http.Request ImportDetail
postUrl config { dataSetName, url, key } =
    (config.baseUrl ++ "/imports/url")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody "application/json" <| encode 0 (encodeImportUrl dataSetName url key))
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
