module Page.SessionDetail exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Algorithm exposing (..)
import Data.Columns as Columns exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.ConfusionMatrix exposing (ConfusionMatrix)
import Data.Context exposing (ContextModel)
import Data.DataFormat as Format
import Data.DataSet exposing (DataSetData, toDataSetName)
import Data.DisplayDate exposing (toShortDateTimeString)
import Data.PredictionDomain as PredictionDomain
import Data.Session exposing (..)
import Data.Status as Status exposing (Status)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed
import Json.Encode
import List exposing (filter, foldr, head)
import List.Extra as List
import Page.Helpers exposing (..)
import Ports
import RemoteData as Remote
import Request.DataSet exposing (getDataByDateRange)
import Request.Log as Log
import Request.Session exposing (..)
import Task
import Util exposing ((=>), formatFloatToString, styledNumber)
import View.Charts as Charts
import View.DeleteDialog as DeleteDialog
import View.Error exposing (viewHttpError)
import View.Extra exposing (viewJust)
import View.Messages as Messages
import View.Pager as Pager exposing (PagedListing, filterToPage, mapToPagedListing)
import Window


type alias Model =
    { sessionId : String
    , sessionResponse : Remote.WebData SessionData
    , resultsResponse : Remote.WebData SessionResults
    , confusionMatrixResponse : Remote.WebData ConfusionMatrix
    , dataSetResponse : Remote.WebData DataSetData
    , deleteDialogModel : Maybe DeleteDialog.Model
    , windowWidth : Int
    , currentPage : Int
    , csvDownload : Remote.WebData String
    }


init : Config -> String -> ( Model, Cmd Msg )
init config sessionId =
    let
        loadModelDetail =
            Request.Session.getOne config sessionId
                |> Remote.sendRequest
                |> Cmd.map SessionResponse

        getWindowWidth =
            Task.attempt GetWindowWidth Window.width
    in
    Model sessionId Remote.Loading Remote.NotAsked Remote.NotAsked Remote.NotAsked Nothing 1140 0 Remote.NotAsked ! [ loadModelDetail, getWindowWidth ]


type Msg
    = SessionResponse (Remote.WebData SessionData)
    | ResultsResponse (Remote.WebData SessionResults)
    | ShowDeleteDialog Model
    | DeleteDialogMsg DeleteDialog.Msg
    | ConfusionMatrixLoaded (Remote.WebData ConfusionMatrix)
    | DataSetLoaded (Remote.WebData DataSetData)
    | GetWindowWidth (Result String Int)
    | ChangePage Int
    | DownloadResults
    | DownloadResponse (Remote.WebData String)


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        SessionResponse response ->
            case response of
                Remote.Success sessionInfo ->
                    if sessionInfo.status == Status.Completed then
                        let
                            details =
                                case sessionInfo.predictionDomain of
                                    PredictionDomain.Classification ->
                                        getConfusionMatrix context.config model.sessionId 0 25
                                            |> Remote.sendRequest
                                            |> Cmd.map ConfusionMatrixLoaded

                                    _ ->
                                        Cmd.none
                        in
                        { model | sessionResponse = response, sessionId = sessionInfo.sessionId }
                            => Cmd.batch
                                [ Request.Session.results context.config model.sessionId 0 1000
                                    |> Remote.sendRequest
                                    |> Cmd.map ResultsResponse
                                , details
                                ]
                    else
                        { model | sessionResponse = response, sessionId = sessionInfo.sessionId }
                            => Cmd.none

                Remote.Failure err ->
                    model => Log.logHttpError err

                _ ->
                    model => Cmd.none

        ResultsResponse response ->
            case Remote.map2 (,) response model.sessionResponse of
                Remote.Success ( results, session ) ->
                    let
                        timeSeriesDataRequest =
                            let
                                dates =
                                    Just ( Maybe.withDefault "" session.startDate, Maybe.withDefault "" session.endDate )
                            in
                            getDataByDateRange context.config (toDataSetName session.dataSourceName) dates
                                |> Remote.sendRequest
                                |> Cmd.map DataSetLoaded

                        cmd =
                            case session.predictionDomain of
                                PredictionDomain.Regression ->
                                    "result-vis"
                                        => Charts.regressionResults results session model.windowWidth
                                        |> List.singleton
                                        |> Json.Encode.object
                                        |> Ports.drawVegaChart

                                PredictionDomain.Forecast ->
                                    timeSeriesDataRequest

                                PredictionDomain.Impact ->
                                    timeSeriesDataRequest

                                _ ->
                                    Cmd.none
                    in
                    { model | resultsResponse = response }
                        => cmd

                Remote.Failure err ->
                    model => Log.logHttpError err

                _ ->
                    model => Cmd.none

        ShowDeleteDialog model ->
            { model | deleteDialogModel = Just (DeleteDialog.init "" model.sessionId) }
                => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                ignoreCascadeParams cmd _ =
                    cmd

                pendingDeleteCmd =
                    Request.Session.delete context.config >> ignoreCascadeParams

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            AppRoutes.modifyUrl AppRoutes.Sessions
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]

        ConfusionMatrixLoaded response ->
            { model | confusionMatrixResponse = response } => Cmd.none

        DataSetLoaded response ->
            case Remote.map3 (,,) model.resultsResponse model.sessionResponse response of
                Remote.Success ( results, session, data ) ->
                    let
                        cmd =
                            case session.predictionDomain of
                                PredictionDomain.Impact ->
                                    "result-vis"
                                        => Charts.impactResults session results data model.windowWidth
                                        |> List.singleton
                                        |> Json.Encode.object
                                        |> Ports.drawVegaChart

                                PredictionDomain.Forecast ->
                                    "result-vis"
                                        => Charts.forecastResults results session data model.windowWidth
                                        |> List.singleton
                                        |> Json.Encode.object
                                        |> Ports.drawVegaChart

                                _ ->
                                    Cmd.none
                    in
                    { model | dataSetResponse = response }
                        => cmd

                Remote.Failure err ->
                    model => Log.logHttpError err

                _ ->
                    model => Cmd.none

        GetWindowWidth result ->
            let
                newWidth =
                    Result.withDefault 1140 result
            in
            { model | windowWidth = newWidth } => Cmd.none

        ChangePage page ->
            { model | currentPage = page } => Cmd.none

        DownloadResults ->
            let
                resultsRequest =
                    Request.Session.resultsCsv context.config model.sessionId
                        |> Remote.sendRequest
                        |> Cmd.map DownloadResponse
            in
            { model | csvDownload = Remote.Loading } => resultsRequest

        DownloadResponse result ->
            case result of
                Remote.Success csv ->
                    { model | csvDownload = result }
                        => Ports.requestSaveFile
                            { contents = csv
                            , contentType = Format.dataFormatToString Format.Csv
                            , name = formatFilename model
                            }

                Remote.Failure err ->
                    { model | csvDownload = result }
                        => Log.logHttpError err

                _ ->
                    model => Cmd.none


formatFilename : Model -> String
formatFilename model =
    model.sessionId ++ "-results." ++ Format.dataFormatToString Format.Csv


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ id "page-header", class "row" ]
            [ div [ class "col-sm-12" ]
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
                ]
            , viewSessionHeader model
            ]
        , viewSessionDetails model
        , viewConfusionMatrix model
        , viewResultsGraph model
        , hr [] []
        , viewResultsTable model
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete Session"
            , bodyMessage = Just "This action cannot be undone but you can always run another session with the same parameters."
            , associatedAssets = []
            }
            |> Html.map DeleteDialogMsg
        ]


viewSessionDetails : Model -> Html Msg
viewSessionDetails model =
    let
        loadingOr =
            loadingOrView model.sessionResponse

        pendingOrCompleted model session =
            if session.status == Status.Completed then
                div []
                    [ viewCompletedSession session
                    , loadingOrView model.resultsResponse viewMetricsList
                    ]
            else
                div []
                    [ viewPendingSession session ]
    in
    div [ class "row", id "details" ]
        [ div [ class "col-sm-3" ]
            [ loadingOr (pendingOrCompleted model) ]

        --, p []
        --    [ a [ class "btn btn-xs btn-primary", href "dashboard-session-champion.html" ]
        --        [ text "(TODO) View algorithm contestants" ]
        --    ]
        , div [ class "col-sm-4" ]
            [ loadingOr (viewSessionInfo model)
            ]
        , div [ class "col-sm-5" ]
            [ loadingOr viewMessages
            , loadingOr viewStatusHistory
            ]
        ]


viewSessionInfo : Model -> SessionData -> Html Msg
viewSessionInfo model session =
    div []
        [ p []
            [ strong []
                [ text "Session Type: " ]
            , text <| toString session.predictionDomain
            ]
        , p []
            [ strong [] [ text "Session ID: " ]
            , br [] []
            , span [ class "small" ] [ text session.sessionId, a [] [ i [ class "fa fa-copy color-mediumgray ml5" ] [] ] ]
            ]
        , p []
            [ strong [] [ text "API Endpoint URL: " ]
            , br [] []
            , span [ class "small" ] [ text ("/sessions/" ++ session.sessionId), a [] [ i [ class "fa fa-copy color-mediumgray ml5" ] [] ] ]
            ]
        , p []
            [ deleteSessionButton model
            ]
        ]


viewMessages : SessionData -> Html Msg
viewMessages session =
    div []
        [ p [ attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#messages", attribute "aria-expanded" "false", attribute "aria-controls" "messages" ]
            [ strong [] [ text "Messages" ]
            , i [ class "fa fa-angle-down" ] []
            ]
        , makeCollapsible "messages" <| Messages.viewMessages session.messages
        ]


viewStatusHistory : SessionData -> Html Msg
viewStatusHistory session =
    let
        statusEntry status =
            tr []
                [ td [ class "number small" ]
                    [ text (toShortDateTimeString status.date) ]
                , td [ class "left" ]
                    [ statusDisplay status.status
                    ]
                ]
    in
    div []
        [ p [ attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#status-log", attribute "aria-expanded" "false", attribute "aria-controls" "status-log" ]
            [ strong [] [ text "Status log" ]
            , i [ class "fa fa-angle-down" ] []
            ]
        , makeCollapsible "status-log" <|
            table [ class "table table-striped" ]
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

        disabledOr : Remote.WebData a -> (Maybe a -> Bool -> Html Msg) -> Html Msg
        disabledOr request view =
            case request of
                Remote.Success resp ->
                    view (Just resp) False

                _ ->
                    view Nothing True
    in
    div []
        [ loadingOr viewSessionName
        , div [ class "col-sm-3" ]
            [ div [ class "mt5 right" ]
                [ div
                    [ class "btn-group", attribute "role" "group" ]
                    [ --loadingOr iterateSessionButton TODO: V2 Feature?
                      loadingOr viewPredictButton
                    ]
                ]
            ]
        ]


iterateSessionButton : SessionData -> Html Msg
iterateSessionButton session =
    button [ class "btn btn-primary" ]
        [ i [ class "fa fa-repeat mr5" ]
            []
        , text "Iterate session"
        ]


deleteSessionButton : Model -> Html Msg
deleteSessionButton model =
    button [ class "btn btn-xs btn-primary", onClick (ShowDeleteDialog model) ]
        [ i [ class "fa fa-trash-o mr5" ]
            []
        , text "Delete session"
        ]


viewPredictButton : SessionData -> Html Msg
viewPredictButton session =
    if canPredictSession session then
        a [ class "btn btn-danger", AppRoutes.href (AppRoutes.ModelDetail (Maybe.withDefault "" session.modelId)) ]
            [ text "Predict" ]
    else
        div [] []


viewSessionDetail : SessionData -> Html Msg
viewSessionDetail session =
    if session.status == Status.Completed then
        viewCompletedSession session
    else
        viewPendingSession session


viewPendingSession : SessionData -> Html Msg
viewPendingSession session =
    div []
        [ h5 [ class "mb15" ]
            [ text "Session Status" ]
        , h4 []
            [ statusDisplay session.status ]
        ]


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
        targetColumnFromColumns : SessionData -> Maybe String
        targetColumnFromColumns session =
            session.columns
                |> List.find (\m -> m.role == Columns.Target)
                |> columnName

        targetColumn : SessionData -> Maybe String
        targetColumn session =
            case session.targetColumn of
                Just target ->
                    Just target

                Nothing ->
                    targetColumnFromColumns session

        viewTargetColumn : Maybe String -> Html Msg
        viewTargetColumn targetColumn =
            case targetColumn of
                Just col ->
                    p []
                        [ strong []
                            [ text "Target Column: " ]
                        , text col
                        ]

                Nothing ->
                    div [] []

        columnName : Maybe ColumnMetadata -> Maybe String
        columnName col =
            case col of
                Nothing ->
                    Nothing

                Just c ->
                    Just c.name

        algorithmName : Maybe Algorithm -> String
        algorithmName algo =
            case algo of
                Nothing ->
                    ""

                Just a ->
                    a.name
    in
    div []
        [ modelLink session
        , p []
            [ strong []
                [ text "Source: " ]
            , a [ AppRoutes.href (AppRoutes.DataSetDetail (toDataSetName session.dataSourceName)) ]
                [ text session.dataSourceName ]
            ]
        , viewTargetColumn (targetColumn session)
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
                , styledNumber <| formatFloatToString value
                ]
    in
    div []
        [ p [ class "small", attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#metrics", attribute "aria-expanded" "false", attribute "aria-controls" "metrics" ]
            [ strong []
                [ text "Metrics" ]
            , i [ class "fa fa-angle-down ml5" ] []
            ]
        , ul [ class "collapse small algorithm-metrics", id "metrics" ]
            (Dict.foldr (\key val html -> listMetric key val :: html) [] results.metrics)
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
                [ text "Session ID: " ]
            , text session.sessionId
            , a []
                [ i [ class "fa fa-copy color-mediumGray" ]
                    []
                ]
            ]
        ]


viewResultsGraph : Model -> Html Msg
viewResultsGraph model =
    div [ class "col-sm-12" ]
        [ Html.Keyed.node "div" [ class "center" ] [ ( "result-vis", div [ id "result-vis" ] [] ) ] ]


viewResultsTable : Model -> Html Msg
viewResultsTable model =
    case model.sessionResponse of
        Remote.Success sessionResponse ->
            if
                (sessionResponse.predictionDomain == PredictionDomain.Forecast)
                    || (sessionResponse.predictionDomain == PredictionDomain.Impact)
            then
                div [] []
            else if sessionResponse.predictionDomain == PredictionDomain.Anomalies then
                viewAnomalyResults model sessionResponse
            else
                viewModelTrainingResults model sessionResponse

        _ ->
            div [] []


viewModelTrainingResults : Model -> SessionData -> Html Msg
viewModelTrainingResults model sessionData =
    let
        targetColumn =
            sessionData.columns
                |> List.find (\c -> c.role == Columns.Target)

        pagedData =
            Remote.map (.data >> mapToPagedListing model.currentPage) model.resultsResponse
    in
    case targetColumn of
        Just target ->
            let
                renderRow datum =
                    let
                        actual =
                            Dict.get (target.name ++ ":actual") datum

                        predicted =
                            Dict.get target.name datum
                    in
                    tr []
                        [ td [ class "left number" ] [ viewJust (\a -> text a) actual ]
                        , td [ class "left number" ] [ viewJust (\p -> text p) predicted ]
                        ]
            in
            div [ class "row" ]
                [ div [ class "col-sm-12" ]
                    [ div [ class "row" ]
                        [ div [ class "col-sm-9" ] [ h3 [] [ text "Test Data" ] ]
                        , div [ class "col-sm-3" ] [ div [ class "mt5 right" ] [ button [ class "btn btn-danger", onClick DownloadResults ] [ text "Download Results" ] ] ]
                        ]
                    , table [ class "table table-striped" ]
                        [ thead []
                            [ tr []
                                [ th [ class "left" ] [ text <| target.name ++ " - Actual" ]
                                , th [ class "left" ] [ text <| target.name ++ " - Predicted" ]
                                ]
                            ]
                        , tbody [] (List.map renderRow (filterToPage pagedData))
                        ]
                    , div [ class "center" ] [ Pager.view pagedData ChangePage ]
                    ]
                ]

        Nothing ->
            div [] []


viewAnomalyResults : Model -> SessionData -> Html Msg
viewAnomalyResults model sessionData =
    let
        pagedData =
            Remote.map (.data >> mapToPagedListing model.currentPage) model.resultsResponse

        otherValueColumns =
            List.filter (\c -> c.name /= "anomaly") sessionData.columns

        renderRow datum =
            let
                anomalyScore =
                    Dict.get "anomaly" datum
            in
            tr []
                (td [ class "left number" ] [ viewJust (\a -> text a) anomalyScore ]
                    :: (otherValueColumns
                            |> List.map (\c -> Dict.get c.name datum)
                            |> List.map (\v -> td [ class "left number" ] [ viewJust (\a -> text a) v ])
                       )
                )
    in
    div [ class "row" ]
        [ div [ class "col-sm-12" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-9" ] [ h3 [] [ text "Test Data" ] ]
                , div [ class "col-sm-3" ] [ div [ class "mt5 right" ] [ button [ class "btn btn-danger", onClick DownloadResults ] [ text "Download Results" ] ] ]
                ]
            , table [ class "table table-striped" ]
                [ thead []
                    [ tr []
                        (th [ class "left" ] [ text "Anomaly" ]
                            :: (otherValueColumns |> List.map (\c -> th [ class "left" ] [ text c.name ]))
                        )
                    ]
                , tbody [] (List.map renderRow (filterToPage pagedData))
                ]
            , div [ class "center" ] [ Pager.view pagedData ChangePage ]
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


viewConfusionMatrix : Model -> Html Msg
viewConfusionMatrix model =
    case model.confusionMatrixResponse of
        Remote.NotAsked ->
            div [] []

        Remote.Loading ->
            div [] []

        Remote.Success response ->
            Charts.renderConfusionMatrix response

        Remote.Failure error ->
            viewHttpError error
