module Request.Session exposing (ForecastSessionRequest, ImpactSessionRequest, ModelSessionRequest, delete, get, getConfusionMatrix, getForDataset, getOne, postForecast, postImpact, postModel, results)

import Data.Columns exposing (ColumnMetadata, encodeColumnMetadataList)
import Data.Config as Config exposing (Config, withAuthorization)
import Data.ConfusionMatrix exposing (..)
import Data.DataSet exposing (DataSetName, dataSetNameToString)
import Data.PredictionDomain exposing (PredictionDomain)
import Data.Session exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Encode as Encode
import Time.DateTime exposing (DateTime, toISO8601)


get : Config -> Int -> Int -> Http.Request SessionList
get { baseUrl, token } page pageSize =
    let
        params =
            pageParams page pageSize
    in
    (baseUrl ++ "/sessions")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeSessionList
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


results : Config -> String -> Int -> Int -> Http.Request SessionResults
results { baseUrl, token } sessionId page pageSize =
    let
        params =
            pageParams page pageSize
    in
    (baseUrl ++ "/sessions/" ++ sessionId ++ "/results")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeSessionResults
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]


getConfusionMatrix : Config -> String -> Int -> Int -> Http.Request ConfusionMatrix
getConfusionMatrix { baseUrl, token } sessionId page pageSize =
    let
        params =
            pageParams page pageSize
    in
    (baseUrl ++ "/sessions/" ++ sessionId ++ "/results/confusionmatrix")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeConfusionMatrix
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


getForDataset : Config -> DataSetName -> Http.Request SessionList
getForDataset { baseUrl, token } dataSetName =
    let
        params =
            [ ( "dataSetName", dataSetNameToString dataSetName ) ]
    in
    (baseUrl ++ "/sessions")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeSessionList
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


delete : Config -> String -> Http.Request ()
delete { baseUrl, token } sessionId =
    (baseUrl ++ "/sessions/" ++ sessionId)
        |> HttpBuilder.delete
        |> withAuthorization token
        |> HttpBuilder.toRequest


getOne : Config -> String -> Http.Request SessionData
getOne { baseUrl, token } sessionId =
    (baseUrl ++ "/sessions/" ++ sessionId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeSession
        |> withAuthorization token
        |> HttpBuilder.toRequest


type alias ModelSessionRequest =
    { name : String
    , dataSourceName : DataSetName
    , columns : List ColumnMetadata
    , predictionDomain : PredictionDomain
    , balance : Maybe Bool
    , containsAnomalies : Maybe Bool
    }


postModel : Config -> ModelSessionRequest -> Http.Request SessionData
postModel { baseUrl, token } sessionRequest =
    let
        requestBody =
            encodeModelSessionRequest sessionRequest
    in
    (baseUrl ++ "/sessions/model")
        |> HttpBuilder.post
        |> HttpBuilder.withExpectJson decodeSession
        |> withAuthorization token
        |> HttpBuilder.withJsonBody requestBody
        |> HttpBuilder.toRequest


encodeModelSessionRequest : ModelSessionRequest -> Encode.Value
encodeModelSessionRequest sessionRequest =
    Encode.object
        [ ( "dataSourceName", Encode.string <| dataSetNameToString <| sessionRequest.dataSourceName )
        , ( "name", Encode.string <| sessionRequest.name )
        , ( "columns", encodeColumnMetadataList <| sessionRequest.columns )
        , ( "predictionDomain", Encode.string <| toString <| sessionRequest.predictionDomain )
        , ( "extraParameters", encodeExtraParameters <| sessionRequest )
        ]


type alias ForecastSessionRequest =
    { name : String
    , dataSourceName : DataSetName
    , columns : List ColumnMetadata
    , dates :
        { startDate : DateTime
        , endDate : DateTime
        }
    , resultInterval : ResultInterval
    }


postForecast : Config -> ForecastSessionRequest -> Http.Request SessionData
postForecast { baseUrl, token } sessionRequest =
    let
        requestBody =
            encodeForecastSessionRequest sessionRequest
    in
    (baseUrl ++ "/sessions/forecast")
        |> HttpBuilder.post
        |> HttpBuilder.withExpectJson decodeSession
        |> withAuthorization token
        |> HttpBuilder.withJsonBody requestBody
        |> HttpBuilder.toRequest


encodeForecastSessionRequest : ForecastSessionRequest -> Encode.Value
encodeForecastSessionRequest sessionRequest =
    Encode.object
        [ ( "dataSourceName", Encode.string <| dataSetNameToString <| sessionRequest.dataSourceName )
        , ( "name", Encode.string <| sessionRequest.name )
        , ( "columns", encodeColumnMetadataList <| sessionRequest.columns )
        , ( "startDate", Encode.string <| toISO8601 <| sessionRequest.dates.startDate )
        , ( "endDate", Encode.string <| toISO8601 <| sessionRequest.dates.endDate )
        , ( "resultInterval", Encode.string <| toString <| sessionRequest.resultInterval )
        ]


type alias ImpactSessionRequest =
    { name : String
    , dataSourceName : DataSetName
    , columns : List ColumnMetadata
    , dates :
        { startDate : DateTime
        , endDate : DateTime
        }
    , eventName : String
    , resultInterval : ResultInterval
    }


postImpact : Config -> ImpactSessionRequest -> Http.Request SessionData
postImpact { baseUrl, token } sessionRequest =
    let
        requestBody =
            encodeImpactSessionRequest sessionRequest
    in
    (baseUrl ++ "/sessions/impact")
        |> HttpBuilder.post
        |> HttpBuilder.withExpectJson decodeSession
        |> withAuthorization token
        |> HttpBuilder.withJsonBody requestBody
        |> HttpBuilder.toRequest


encodeImpactSessionRequest : ImpactSessionRequest -> Encode.Value
encodeImpactSessionRequest sessionRequest =
    Encode.object
        [ ( "dataSourceName", Encode.string <| dataSetNameToString <| sessionRequest.dataSourceName )
        , ( "name", Encode.string <| sessionRequest.name )
        , ( "columns", encodeColumnMetadataList <| sessionRequest.columns )
        , ( "startDate", Encode.string <| toISO8601 <| sessionRequest.dates.startDate )
        , ( "endDate", Encode.string <| toISO8601 <| sessionRequest.dates.endDate )
        , ( "eventName", Encode.string <| sessionRequest.eventName )
        , ( "resultInterval", Encode.string <| toString <| sessionRequest.resultInterval )
        ]


encodeExtraParameters : ModelSessionRequest -> Encode.Value
encodeExtraParameters sessionRequest =
    let
        balance =
            sessionRequest.balance
                |> Maybe.map Encode.bool
                |> Maybe.withDefault Encode.null

        anomalies =
            sessionRequest.containsAnomalies
                |> Maybe.map Encode.bool
                |> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "balance", balance )
        , ( "containsAnomalies", anomalies )
        ]
