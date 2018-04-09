module Request.Model exposing (delete, get, getOne, predict, predictRaw)

import Data.Config as Config exposing (Config, withAppHeader)
import Data.Model exposing (ModelData, ModelList, PredictionResult, decodeModel, decodeModelList, decodePredictions)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Nexosis exposing (ClientConfig, withAuthorization)
import Request.Sorting exposing (SortParameters, sortParams)


get : Config -> Int -> Int -> SortParameters -> Http.Request ModelList
get config page pageSize sorting =
    let
        params =
            pageParams page pageSize
                ++ sortParams sorting
    in
    (config.clientConfig.url ++ "/models")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeModelList
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]


delete : Config -> String -> Http.Request ()
delete config modelId =
    (config.clientConfig.url ++ "/models/" ++ modelId)
        |> HttpBuilder.delete
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


getOne : Config -> String -> Http.Request ModelData
getOne config modelId =
    (config.clientConfig.url ++ "/models/" ++ modelId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeModel
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.toRequest


predict : Config -> String -> String -> String -> Http.Request PredictionResult
predict config modelId content contentType =
    (config.clientConfig.url ++ "/models/" ++ modelId ++ "/predict")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody contentType content)
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.withExpectJson decodePredictions
        |> HttpBuilder.toRequest


predictRaw : Config -> String -> String -> String -> String -> Http.Request String
predictRaw config modelId content uploadType contentType =
    (config.clientConfig.url ++ "/models/" ++ modelId ++ "/predict")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody uploadType content)
        |> HttpBuilder.withHeader "Accept" contentType
        |> withAuthorization config.clientConfig
        |> withAppHeader config
        |> HttpBuilder.withExpectString
        |> HttpBuilder.toRequest
