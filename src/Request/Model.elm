module Request.Model exposing (delete, get, getOne, predict, predictRaw)

import Data.Config as Config exposing (Config)
import Data.Model exposing (ModelData, ModelList, PredictionResult, decodeModel, decodeModelList, decodePredictions)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Nexosis exposing (ClientConfig, getBaseUrl)
import NexosisHelpers exposing (addHeaders)
import Request.Sorting exposing (SortParameters, sortParams)


get : Config -> Int -> Int -> SortParameters -> Http.Request ModelList
get config page pageSize sorting =
    let
        params =
            pageParams page pageSize
                ++ sortParams sorting
    in
    (getBaseUrl config.clientConfig ++ "/models")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeModelList
        |> HttpBuilder.withQueryParams params
        |> addHeaders config.clientConfig
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]


delete : Config -> String -> Http.Request ()
delete config modelId =
    (getBaseUrl config.clientConfig ++ "/models/" ++ modelId)
        |> HttpBuilder.delete
        |> addHeaders config.clientConfig
        |> HttpBuilder.toRequest


getOne : Config -> String -> Http.Request ModelData
getOne config modelId =
    (getBaseUrl config.clientConfig ++ "/models/" ++ modelId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeModel
        |> addHeaders config.clientConfig
        |> HttpBuilder.toRequest


predict : Config -> String -> String -> String -> Http.Request PredictionResult
predict config modelId content contentType =
    (getBaseUrl config.clientConfig ++ "/models/" ++ modelId ++ "/predict")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody contentType content)
        |> addHeaders config.clientConfig
        |> HttpBuilder.withExpectJson decodePredictions
        |> HttpBuilder.toRequest


predictRaw : Config -> String -> String -> String -> String -> Http.Request String
predictRaw config modelId content uploadType contentType =
    (getBaseUrl config.clientConfig ++ "/models/" ++ modelId ++ "/predict")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody uploadType content)
        |> HttpBuilder.withHeader "Accept" contentType
        |> addHeaders config.clientConfig
        |> HttpBuilder.withExpectString
        |> HttpBuilder.toRequest
