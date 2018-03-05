module Request.Session exposing (ModelSessionRequest, delete, get, getForDataset, getOne, postForecast, postModel, results)

import Data.Columns exposing (ColumnMetadata)
import Data.Config as Config exposing (Config, withAuthorization)
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
        expect =
            decodeSessionList
                |> Http.expectJson

        params =
            pageParams page pageSize
    in
    (baseUrl ++ "/sessions")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


results : Config -> String -> Int -> Int -> Http.Request SessionResults
results { baseUrl, token } sessionId page pageSize =
    let
        expect =
            decodeSessionResults
                |> Http.expectJson

        params =
            pageParams page pageSize
    in
    (baseUrl ++ "/sessions/" ++ sessionId ++ "/results")
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


getForDataset : Config -> DataSetName -> Http.Request SessionList
getForDataset { baseUrl, token } dataSetName =
    let
        expect =
            decodeSessionList
                |> Http.expectJson

        params =
            [ ( "dataSetName", dataSetNameToString dataSetName ) ]
    in
    (baseUrl ++ "/sessions")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


delete : Config -> String -> Http.Request ()
delete { baseUrl, token } sessionId =
    (baseUrl ++ "/sessions/" ++ sessionId)
        |> HttpBuilder.delete
        |> withAuthorization token
        |> HttpBuilder.toRequest


expectSessionData : Http.Expect SessionData
expectSessionData =
    decodeSession
        |> Http.expectJson


getOne : Config -> String -> Http.Request SessionData
getOne { baseUrl, token } sessionId =
    (baseUrl ++ "/sessions/" ++ sessionId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expectSessionData
        |> withAuthorization token
        |> HttpBuilder.toRequest


type alias ModelSessionRequest =
    { name : String
    , dataSourceName : DataSetName
    , columns : List ColumnMetadata
    , targetColumn : Maybe String
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
        |> HttpBuilder.withExpect expectSessionData
        |> withAuthorization token
        |> HttpBuilder.withJsonBody requestBody
        |> HttpBuilder.toRequest


encodeModelSessionRequest : ModelSessionRequest -> Encode.Value
encodeModelSessionRequest sessionRequest =
    Encode.object
        [ ( "dataSourceName", Encode.string <| dataSetNameToString <| sessionRequest.dataSourceName )
        , ( "name", Encode.string <| sessionRequest.name )
        , ( "columns", encodeColumnMetadataList <| sessionRequest.columns )
        , ( "targetColumn"
          , case sessionRequest.targetColumn of
                Just target ->
                    Encode.string <| target

                Nothing ->
                    Encode.null
          )
        , ( "predictionDomain", Encode.string <| toString <| sessionRequest.predictionDomain )
        , ( "extraParameters", encodeExtraParameters <| sessionRequest )
        ]


type alias ForecastSessionRequest =
    { name : String
    , dataSourceName : DataSetName
    , columns : List ColumnMetadata
    , targetColumn : String
    , startDate : DateTime
    , endDate : DateTime
    }


postForecast : Config -> ForecastSessionRequest -> Http.Request SessionData
postForecast { baseUrl, token } sessionRequest =
    let
        requestBody =
            encodeForecastSessionRequest sessionRequest
    in
    (baseUrl ++ "/sessions/forecast")
        |> HttpBuilder.post
        |> HttpBuilder.withExpect expectSessionData
        |> withAuthorization token
        |> HttpBuilder.withJsonBody requestBody
        |> HttpBuilder.toRequest


encodeForecastSessionRequest : ForecastSessionRequest -> Encode.Value
encodeForecastSessionRequest sessionRequest =
    Encode.object
        [ ( "dataSourceName", Encode.string <| dataSetNameToString <| sessionRequest.dataSourceName )
        , ( "name", Encode.string <| sessionRequest.name )
        , ( "columns", encodeColumnMetadataList <| sessionRequest.columns )
        , ( "targetColumn", Encode.string <| sessionRequest.targetColumn )
        , ( "startDate", Encode.string <| toISO8601 <| sessionRequest.startDate )
        , ( "endDate", Encode.string <| toISO8601 <| sessionRequest.endDate )
        ]


encodeColumnMetadataList : List ColumnMetadata -> Encode.Value
encodeColumnMetadataList columns =
    Encode.object <|
        (columns
            |> List.map (\c -> ( c.name, encodeColumnValues c ))
        )


encodeColumnValues : ColumnMetadata -> Encode.Value
encodeColumnValues column =
    Encode.object
        [ ( "dataType", Encode.string <| toString <| column.dataType )
        , ( "role", Encode.string <| toString <| column.role )
        , ( "imputation", Encode.string <| toString <| column.imputation )
        , ( "aggregation", Encode.string <| toString <| column.aggregation )
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
