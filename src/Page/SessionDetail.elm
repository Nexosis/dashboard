module Page.SessionDetail exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Algorithm exposing (..)
import Data.Columns as Role exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.DataSet exposing (toDataSetName)
import Data.Message exposing (..)
import Data.Session exposing (..)
import Data.Status exposing (Status)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (filter, foldr, head)
import RemoteData as Remote
import Request.Log as Log
import Request.Session exposing (..)
import Util exposing ((=>))


type alias Model =
    { sessionId : String
    , sessionResponse : Remote.WebData SessionData
    , resultsResponse : Remote.WebData SessionResults
    , config : Config
    }


type Msg
    = SessionResponse (Remote.WebData SessionData)
    | ResultsResponse (Remote.WebData SessionResults)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionResponse response ->
            case response of
                Remote.Success sessionInfo ->
                    { model | sessionResponse = response, sessionId = sessionInfo.sessionId }
                        => (Request.Session.results model.config model.sessionId 0 1
                                |> Remote.sendRequest
                                |> Cmd.map ResultsResponse
                           )

                Remote.Failure err ->
                    model => Log.logHttpError err

                _ ->
                    model => Cmd.none

        ResultsResponse response ->
            case response of
                Remote.Success contestInfo ->
                    { model | resultsResponse = response } => Cmd.none

                Remote.Failure err ->
                    model => Log.logHttpError err

                _ ->
                    model => Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ p [ class "breadcrumb" ]
            [ span []
                [ a [ AppRoutes.href AppRoutes.Home ]
                    [ text "API Dashboard" ]
                ]
            , i [ class "fa fa-angle-right", attribute "style" "margin: 0 5px;" ]
                []
            , span []
                [ a [ AppRoutes.href AppRoutes.Sessions ]
                    [ text "Sessions" ]
                ]
            ]
        , viewSessionHeader model
        , hr [] []
        , viewSessionDetails model
        ]


viewSessionDetails : Model -> Html Msg
viewSessionDetails model =
    let
        loadingOr =
            loadingOrView model.sessionResponse

        pendingOrCompleted session =
            if session.status == Data.Status.Completed then
                viewCompletedSession session
            else
                viewPendingSession session

        statusHistoryOrMessages session =
            if sessionIsCompleted session then
                viewMessages session
            else
                viewStatusHistory session
    in
    div [ class "row" ]
        [ div [ class "col-sm-4" ]
            [ loadingOr pendingOrCompleted
            , loadingOrView model.resultsResponse viewMetricsList

            --, p []
            --    [ a [ class "btn btn-xs secondary", href "dashboard-session-champion.html" ]
            --        [ text "(TODO) View algorithm contestants" ]
            --    ]
            ]
        , div [ class "col-sm-5" ]
            [ --loadingOr statusHistoryOrMessages
              loadingOr viewMessages
            , loadingOr viewStatusHistory
            ]
        , div [ class "col-sm-3" ]
            []
        ]


viewMessages : SessionData -> Html Msg
viewMessages session =
    let
        labelType : Message -> String
        labelType message =
            case message.severity of
                Debug ->
                    "info"

                Informational ->
                    "info"

                Warning ->
                    "warning"

                Error ->
                    "danger"

        messageEntry : Message -> Html Msg
        messageEntry message =
            tr []
                [ td [ class "per10 left" ]
                    [ span [ class ("label label-" ++ labelType message ++ " mr5") ]
                        [ text (toString message.severity) ]
                    ]
                , td [ class "left" ]
                    [ text message.message ]
                ]
    in
    div []
        [ h5 [ class "mt15 mb15" ]
            [ text "Messages" ]
        , table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ class "per10" ]
                        [ text "Date" ]
                    , th [ class "per15" ]
                        [ text "Status" ]
                    ]
                ]
            , tbody []
                (List.map messageEntry session.messages)
            ]
        ]


viewStatusHistory : SessionData -> Html Msg
viewStatusHistory session =
    let
        labelType status =
            case status.status of
                Data.Status.Requested ->
                    "info"

                Data.Status.Started ->
                    "warning"

                Data.Status.CancellationPending ->
                    "warning"

                Data.Status.Cancelled ->
                    "warning"

                Data.Status.Completed ->
                    "success"

                Data.Status.Failed ->
                    "error"

        statusEntry status =
            tr []
                [ td [ class "small" ]
                    [ text status.date ]
                , td [ class "left" ]
                    [ span [ class ("label label-" ++ labelType status ++ " mr5") ]
                        [ text (toString status.status) ]
                    ]
                ]
    in
    div []
        [ h5 [ class "mt15 mb15" ]
            [ text "Status Log" ]
        , table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ class "per10" ]
                        [ text "Date" ]
                    , th [ class "per15" ]
                        [ text "Status" ]
                    ]
                ]
            , tbody []
                (List.map statusEntry session.statusHistory)
            ]
        ]


viewSessionHeader : Model -> Html Msg
viewSessionHeader model =
    let
        loadingOr =
            loadingOrView model.sessionResponse
    in
    div []
        [ div [ class "row" ]
            [ loadingOr viewSessionName
            , div [ class "col-sm-3" ]
                [ div [ class "mt10 right" ]
                    [ loadingOr viewPredictButton
                    ]
                ]
            ]
        , div [ class "row" ]
            [ loadingOr viewSessionId
            , div [ class "col-sm-4" ]
                [ p [ class "small" ]
                    [ strong []
                        [ text "Session Type:" ]
                    , text "Classification"
                    ]
                ]
            , div [ class "col-sm-4 right" ]
                [ button [ class "btn btn-xs other" ]
                    [ i [ class "fa fa-repeat mr5" ]
                        []
                    , text "(TODO) Iterate session"
                    ]
                , button [ class "btn btn-xs secondary" ]
                    [ i [ class "fa fa-trash-o mr5" ]
                        []
                    , text "(TODO) Delete"
                    ]
                ]
            ]
        ]


viewPredictButton : SessionData -> Html Msg
viewPredictButton session =
    if canPredictSession session then
        button [ class "btn" ]
            [ text "(TODO) Predict" ]
    else
        div [] []


viewSessionDetail : SessionData -> Html Msg
viewSessionDetail session =
    if session.status == Data.Status.Completed then
        viewCompletedSession session
    else
        viewPendingSession session


viewPendingSession : SessionData -> Html Msg
viewPendingSession session =
    h5 [ class "mb15" ]
        [ text "Session Status" ]


modelLink : SessionData -> Html Msg
modelLink session =
    case session.modelId of
        Nothing ->
            div [] []

        Just modelId ->
            p []
                [ strong []
                    [ text "Model: " ]
                , a [ AppRoutes.href (AppRoutes.ModelDetail modelId) ]
                    [ text session.name ]
                ]


viewCompletedSession : SessionData -> Html Msg
viewCompletedSession session =
    let
        isTarget : ColumnMetadata -> Bool
        isTarget column =
            column.role == Role.Target

        targetColumnFromColumns : SessionData -> String
        targetColumnFromColumns session =
            filter isTarget session.columns
                |> head
                |> columnName

        targetColumn : SessionData -> String
        targetColumn session =
            case session.targetColumn of
                Just target ->
                    target

                Nothing ->
                    targetColumnFromColumns session

        columnName : Maybe ColumnMetadata -> String
        columnName col =
            case col of
                Nothing ->
                    ""

                Just c ->
                    c.name

        algorithmName : Maybe Algorithm -> String
        algorithmName algo =
            case algo of
                Nothing ->
                    ""

                Just a ->
                    a.name
    in
    div []
        [ h5 [ class "mt15 mb15" ]
            [ text "Details" ]
        , modelLink session
        , p []
            [ strong []
                [ text "Source: " ]
            , a [ AppRoutes.href (AppRoutes.DataSetDetail (toDataSetName session.dataSourceName)) ]
                [ text session.dataSourceName ]
            ]
        , p []
            [ strong []
                [ text "Target Column: " ]
            , text (targetColumn session)
            ]
        , p []
            [ strong []
                [ text "Algorithm: " ]
            , text (algorithmName session.algorithm)
            ]
        ]


viewMetricsList : SessionResults -> Html Msg
viewMetricsList results =
    let
        listMetric key value =
            li []
                [ strong []
                    [ text key ]
                , br []
                    []
                , text value
                ]
    in
    div []
        [ p [ class "small" ]
            [ strong []
                [ text "Metrics" ]
            ]
        , ul [ class "small algorithm-metrics" ]
            (Dict.foldr (\key val html -> listMetric key (toString val) :: html) [] results.metrics)
        ]


viewSessionName : SessionData -> Html Msg
viewSessionName session =
    div [ class "col-sm-9" ]
        [ h2 [ class "mt10" ] [ text session.name ]
        ]


viewSessionId : SessionData -> Html Msg
viewSessionId session =
    div [ class "col-sm-4" ]
        [ p [ class "small" ]
            [ strong []
                [ text "Session ID:" ]
            , text session.sessionId
            , a []
                [ i [ class "fa fa-copy color-mediumGray" ]
                    []
                ]
            ]
        ]


loadingOrView : Remote.WebData a -> (a -> Html Msg) -> Html Msg
loadingOrView request view =
    case request of
        Remote.Success resp ->
            view resp

        Remote.Loading ->
            div [ class "loading--line" ] []

        _ ->
            div [] []


init : Config -> String -> ( Model, Cmd Msg )
init config sessionId =
    let
        loadModelDetail =
            Request.Session.getOne config sessionId
                |> Remote.sendRequest
                |> Cmd.map SessionResponse
    in
    Model sessionId Remote.Loading Remote.NotAsked config => loadModelDetail
