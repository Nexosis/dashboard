module Request.Import exposing (get, postUrl)

import Data.Config exposing (Config, withAuthorization)
import Data.Import exposing (ImportDetail, decodeImportDetail)
import Http
import HttpBuilder exposing (withExpect)
import Json.Encode exposing (Value, encode)


expectImportDetail : Http.Expect ImportDetail
expectImportDetail =
    decodeImportDetail
        |> Http.expectJson


postUrl : Config -> String -> String -> Http.Request ImportDetail
postUrl { token, baseUrl } dataSetName url =
    (baseUrl ++ "/imports/url")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody "application/json" <| encode 0 (encodeImportUrl dataSetName url))
        |> withAuthorization token
        |> withExpect expectImportDetail
        |> HttpBuilder.toRequest


get : Config -> String -> Http.Request ImportDetail
get { token, baseUrl } importId =
    (baseUrl ++ "/imports/" ++ importId)
        |> HttpBuilder.get
        |> withAuthorization token
        |> withExpect expectImportDetail
        |> HttpBuilder.toRequest


encodeImportUrl : String -> String -> Json.Encode.Value
encodeImportUrl dataSetName url =
    Json.Encode.object
        [ ( "dataSetName", Json.Encode.string <| dataSetName )
        , ( "url", Json.Encode.string <| url )
        ]
