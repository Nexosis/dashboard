module Request.Model exposing (delete, get, getOne, predict, predictRaw)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.Model exposing (ModelData, ModelList, PredictionResult, decodeModel, decodeModelList, decodePredictions)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Request.Sorting exposing (SortParameters, sortParams)


get : Config -> Int -> Int -> SortParameters -> Http.Request ModelList
get config page pageSize sorting =
    let
        params =
            pageParams page pageSize
                ++ sortParams sorting
    in
    (config.baseUrl ++ "/models")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeModelList
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]


delete : Config -> String -> Http.Request ()
delete config modelId =
    (config.baseUrl ++ "/models/" ++ modelId)
        |> HttpBuilder.delete
        |> withAuthorization config
        |> HttpBuilder.toRequest


getOne : Config -> String -> Http.Request ModelData
getOne config modelId =
    (config.baseUrl ++ "/models/" ++ modelId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeModel
        |> withAuthorization config
        |> HttpBuilder.toRequest


predict : Config -> String -> String -> String -> Http.Request PredictionResult
predict config modelId content contentType =
    (config.baseUrl ++ "/models/" ++ modelId ++ "/predict")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody contentType content)
        |> withAuthorization config
        |> HttpBuilder.withExpectJson decodePredictions
        |> HttpBuilder.toRequest


predictRaw : Config -> String -> String -> String -> String -> Http.Request String
predictRaw config modelId content uploadType contentType =
    (config.baseUrl ++ "/models/" ++ modelId ++ "/predict")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody uploadType content)
        |> HttpBuilder.withHeader "Accept" contentType
        |> withAuthorization config
        |> HttpBuilder.withExpectString
        |> HttpBuilder.toRequest
