module Request.Model exposing (delete, get, getOne, predict, predictRaw)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.Model exposing (ModelData, ModelList, PredictionResult, decodeModel, decodeModelList, decodePredictions)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)


get : Config -> Int -> Int -> Http.Request ModelList
get { baseUrl, token } page pageSize =
    let
        expect =
            decodeModelList
                |> Http.expectJson

        params =
            pageParams page pageSize
    in
    (baseUrl ++ "/models")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]


delete : Config -> String -> Http.Request ()
delete { baseUrl, token } modelId =
    (baseUrl ++ "/models/" ++ modelId)
        |> HttpBuilder.delete
        |> withAuthorization token
        |> HttpBuilder.toRequest


getOne : Config -> String -> Http.Request ModelData
getOne { baseUrl, token } modelId =
    let
        expect =
            decodeModel
                |> Http.expectJson
    in
    (baseUrl ++ "/models/" ++ modelId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization token
        |> HttpBuilder.toRequest


expectPredictions : Http.Expect PredictionResult
expectPredictions =
    decodePredictions
        |> Http.expectJson


predict : Config -> String -> String -> String -> Http.Request PredictionResult
predict { baseUrl, token } modelId content contentType =
    (baseUrl ++ "/models/" ++ modelId ++ "/predict")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody contentType content)
        |> withAuthorization token
        |> HttpBuilder.withExpect expectPredictions
        |> HttpBuilder.toRequest


predictRaw : Config -> String -> String -> String -> Http.Request String
predictRaw { baseUrl, token } modelId content contentType =
    (baseUrl ++ "/models/" ++ modelId ++ "/predict")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.stringBody "application/json" content)
        |> HttpBuilder.withHeader "Accept" contentType
        |> withAuthorization token
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest
