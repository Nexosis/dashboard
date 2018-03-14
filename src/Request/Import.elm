module Request.Import exposing (PostUrlRequest, get, postUrl)

import Data.Config exposing (Config, withAuthorization)
import Data.Import exposing (ImportDetail, decodeImportDetail)
import Http
import HttpBuilder exposing (withExpectJson)
import Json.Encode exposing (Value, encode)


type alias PostUrlRequest =
    { dataSetName : String
    , url : String
    }


postUrl : Config -> PostUrlRequest -> Http.Request ImportDetail
postUrl { token, baseUrl } { dataSetName, url } =
    (baseUrl ++ "/imports/url")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody "application/json" <| encode 0 (encodeImportUrl dataSetName url))
        |> withAuthorization token
        |> withExpectJson decodeImportDetail
        |> HttpBuilder.toRequest


get : Config -> String -> Http.Request ImportDetail
get { token, baseUrl } importId =
    (baseUrl ++ "/imports/" ++ importId)
        |> HttpBuilder.get
        |> withAuthorization token
        |> withExpectJson decodeImportDetail
        |> HttpBuilder.toRequest


encodeImportUrl : String -> String -> Json.Encode.Value
encodeImportUrl dataSetName url =
    Json.Encode.object
        [ ( "dataSetName", Json.Encode.string <| Http.encodeUri dataSetName )
        , ( "url", Json.Encode.string <| url )
        ]
