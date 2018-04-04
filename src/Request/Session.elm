module Request.Session exposing (ForecastSessionRequest, ImpactSessionRequest, ModelSessionRequest, delete, get, getConfusionMatrix, getDistanceMetrics, getDistanceMetricsCsv, getForDataset, getOne, postForecast, postImpact, postModel, results, resultsCsv)

import Data.Columns exposing (ColumnMetadata, encodeColumnMetadataList)
import Data.Config as Config exposing (Config, withAuthorization)
import Data.ConfusionMatrix exposing (..)
import Data.DataSet exposing (DataSetName, dataSetNameToString)
import Data.DistanceMetric exposing (..)
import Data.PredictionDomain exposing (PredictionDomain)
import Data.Session exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Encode as Encode
import Request.Sorting exposing (SortParameters, sortParams)
import Time.ZonedDateTime exposing (ZonedDateTime, toISO8601)


get : Config -> Int -> Int -> SortParameters -> Http.Request SessionList
get config page pageSize sorting =
    let
        params =
            pageParams page pageSize
                ++ sortParams sorting
    in
    (config.baseUrl ++ "/sessions")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeSessionList
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest


results : Config -> String -> Int -> Int -> Http.Request SessionResults
results config sessionId page pageSize =
    let
        params =
            pageParams page pageSize
    in
    (config.baseUrl ++ "/sessions/" ++ sessionId ++ "/results")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeSessionResults
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest


resultsCsv : Config -> String -> Http.Request String
resultsCsv config sessionId =
    (config.baseUrl ++ "/sessions/" ++ sessionId ++ "/results")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectString
        |> HttpBuilder.withHeader "Accept" "text/csv"
        |> HttpBuilder.withQueryParams [ ( "pageSize", "1000" ) ]
        |> withAuthorization config
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]


getConfusionMatrix : Config -> String -> Int -> Int -> Http.Request ConfusionMatrix
getConfusionMatrix config sessionId page pageSize =
    let
        params =
            pageParams page pageSize
    in
    (config.baseUrl ++ "/sessions/" ++ sessionId ++ "/results/confusionmatrix")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeConfusionMatrix
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest


getForDataset : Config -> DataSetName -> Http.Request SessionList
getForDataset config dataSetName =
    let
        params =
            [ ( "dataSetName", dataSetNameToString dataSetName ) ]
    in
    (config.baseUrl ++ "/sessions")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeSessionList
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest


delete : Config -> String -> Http.Request ()
delete config sessionId =
    (config.baseUrl ++ "/sessions/" ++ sessionId)
        |> HttpBuilder.delete
        |> withAuthorization config
        |> HttpBuilder.toRequest


getOne : Config -> String -> Http.Request SessionData
getOne config sessionId =
    (config.baseUrl ++ "/sessions/" ++ sessionId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeSession
        |> withAuthorization config
        |> HttpBuilder.toRequest


type alias ModelSessionRequest =
    { name : Maybe String
    , dataSourceName : DataSetName
    , columns : List ColumnMetadata
    , predictionDomain : PredictionDomain
    , balance : Maybe Bool
    , containsAnomalies : Maybe Bool
    }


postModel : Config -> ModelSessionRequest -> Http.Request SessionData
postModel config sessionRequest =
    let
        requestBody =
            encodeModelSessionRequest sessionRequest
    in
    (config.baseUrl ++ "/sessions/model")
        |> HttpBuilder.post
        |> HttpBuilder.withExpectJson decodeSession
        |> withAuthorization config
        |> HttpBuilder.withJsonBody requestBody
        |> HttpBuilder.toRequest


encodeModelSessionRequest : ModelSessionRequest -> Encode.Value
encodeModelSessionRequest sessionRequest =
    Encode.object
        [ ( "dataSourceName", Encode.string <| dataSetNameToString <| sessionRequest.dataSourceName )
        , ( "name", encodeName sessionRequest.name )
        , ( "columns", encodeColumnMetadataList <| sessionRequest.columns )
        , ( "predictionDomain", Encode.string <| toString <| sessionRequest.predictionDomain )
        , ( "extraParameters", encodeExtraParameters <| sessionRequest )
        ]


type alias ForecastSessionRequest =
    { name : Maybe String
    , dataSourceName : DataSetName
    , columns : List ColumnMetadata
    , dates :
        { startDate : ZonedDateTime
        , endDate : ZonedDateTime
        }
    , resultInterval : ResultInterval
    }


postForecast : Config -> ForecastSessionRequest -> Http.Request SessionData
postForecast config sessionRequest =
    let
        requestBody =
            encodeForecastSessionRequest sessionRequest
    in
    (config.baseUrl ++ "/sessions/forecast")
        |> HttpBuilder.post
        |> HttpBuilder.withExpectJson decodeSession
        |> withAuthorization config
        |> HttpBuilder.withJsonBody requestBody
        |> HttpBuilder.toRequest


encodeForecastSessionRequest : ForecastSessionRequest -> Encode.Value
encodeForecastSessionRequest sessionRequest =
    Encode.object
        [ ( "dataSourceName", Encode.string <| dataSetNameToString <| sessionRequest.dataSourceName )
        , ( "name", encodeName sessionRequest.name )
        , ( "columns", encodeColumnMetadataList <| sessionRequest.columns )
        , ( "startDate", Encode.string <| toISO8601 <| sessionRequest.dates.startDate )
        , ( "endDate", Encode.string <| toISO8601 <| sessionRequest.dates.endDate )
        , ( "resultInterval", Encode.string <| toString <| sessionRequest.resultInterval )
        ]


type alias ImpactSessionRequest =
    { name : Maybe String
    , dataSourceName : DataSetName
    , columns : List ColumnMetadata
    , dates :
        { startDate : ZonedDateTime
        , endDate : ZonedDateTime
        }
    , eventName : String
    , resultInterval : ResultInterval
    }


postImpact : Config -> ImpactSessionRequest -> Http.Request SessionData
postImpact config sessionRequest =
    let
        requestBody =
            encodeImpactSessionRequest sessionRequest
    in
    (config.baseUrl ++ "/sessions/impact")
        |> HttpBuilder.post
        |> HttpBuilder.withExpectJson decodeSession
        |> withAuthorization config
        |> HttpBuilder.withJsonBody requestBody
        |> HttpBuilder.toRequest


encodeImpactSessionRequest : ImpactSessionRequest -> Encode.Value
encodeImpactSessionRequest sessionRequest =
    Encode.object
        [ ( "dataSourceName", Encode.string <| dataSetNameToString <| sessionRequest.dataSourceName )
        , ( "name", encodeName sessionRequest.name )
        , ( "columns", encodeColumnMetadataList <| sessionRequest.columns )
        , ( "startDate", Encode.string <| toISO8601 <| sessionRequest.dates.startDate )
        , ( "endDate", Encode.string <| toISO8601 <| sessionRequest.dates.endDate )
        , ( "eventName", Encode.string <| sessionRequest.eventName )
        , ( "resultInterval", Encode.string <| toString <| sessionRequest.resultInterval )
        ]


encodeName : Maybe String -> Encode.Value
encodeName name =
    name
        |> Maybe.map Encode.string
        |> Maybe.withDefault Encode.null


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


getDistanceMetricsCsv : Config -> String -> Int -> Int -> Http.Request String
getDistanceMetricsCsv config sessionId page pageSize =
    let
        params =
            pageParams page pageSize
    in
    (config.baseUrl ++ "/sessions/" ++ sessionId ++ "/results/mahalanobisdistances")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectString
        |> HttpBuilder.withHeader "Accept" "text/csv"
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest


getDistanceMetrics : Config -> String -> Int -> Int -> Http.Request DistanceMetrics
getDistanceMetrics config sessionId page pageSize =
    let
        params =
            pageParams page pageSize
    in
    (config.baseUrl ++ "/sessions/" ++ sessionId ++ "/results/mahalanobisdistances")
        |> HttpBuilder.get
        |> HttpBuilder.withExpectJson decodeDistanceMetrics
        |> HttpBuilder.withQueryParams params
        |> withAuthorization config
        |> HttpBuilder.toRequest
