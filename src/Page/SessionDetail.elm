module Page.SessionDetail exposing (Model, Msg, SessionDateData, getDataDateRange, init, update, view)

import AppRoutes
import Data.Context exposing (ContextModel, contextToAuth)
import Data.DataFormat as Format
import Data.DisplayDate exposing (toShortDateTimeString, toShortTimeString)
import Data.Metric exposing (getMetricDescriptionFromKey, getMetricNameFromKey)
import Data.Session exposing (canPredictSession, sessionIsCompleted)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import List exposing (filter, foldr, head)
import List.Extra as List
import Nexosis.Api.Data exposing (getDataByDateRange)
import Nexosis.Api.Messages exposing (..)
import Nexosis.Api.Sessions exposing (..)
import Nexosis.Types.Algorithm exposing (..)
import Nexosis.Types.Columns as Columns exposing (ColumnMetadata, Role)
import Nexosis.Types.ConfusionMatrix exposing (ConfusionMatrix)
import Nexosis.Types.DataSet exposing (DataSetData, toDataSetName)
import Nexosis.Types.DistanceMetric exposing (..)
import Nexosis.Types.Message exposing (..)
import Nexosis.Types.PredictionDomain as PredictionDomain
import Nexosis.Types.Session exposing (..)
import Nexosis.Types.Status as Status exposing (Status)
import Page.Helpers exposing (..)
import Ports
import RemoteData as Remote
import Request.Log as Log
import Task
import Time.DateTime as DateTime exposing (DateTime)
import Time.ZonedDateTime exposing (fromDateTime)
import Util exposing ((=>), delayTask, formatDateWithTimezone, formatDisplayName, formatFloatToString, getTimezoneFromDate, spinner, styledNumber)
import View.Breadcrumb as Breadcrumb
import View.Charts as Charts
import View.CopyableText exposing (copyableText)
import View.DeleteDialog as DeleteDialog
import View.Error exposing (viewHttpError)
import View.Extra exposing (viewIf, viewJust)
import View.Messages as Messages
import View.Modal as Modal
import View.Pager as Pager exposing (PagedListing, filterToPage, mapToPagedListing)
import View.Tooltip exposing (helpIconFromText)
import Window


type alias Model =
    { sessionId : String
    , loadingResponse : Remote.WebData SessionData
    , resultsResponse : Remote.WebData SessionResults
    , confusionMatrixResponse : Remote.WebData ConfusionMatrix
    , dataSetResponse : Remote.WebData DataSetData
    , deleteDialogModel : Maybe DeleteDialog.Model
    , windowWidth : Int
    , currentPage : Int
    , csvDownload : Remote.WebData String
    , predictionDomain : Maybe PredictionDomain.PredictionDomain
    , howLongModalModel : Maybe (Modal.Config Msg)
    , distanceMetricsResponse : Remote.WebData DistanceMetrics
    , messagesResponse : Remote.WebData MessageList
    }


init : ContextModel -> String -> ( Model, Cmd Msg )
init context sessionId =
    let
        getWindowWidth =
            Task.attempt GetWindowWidth Window.width

        loadSessionDetail =
            Nexosis.Api.Sessions.getOne (contextToAuth context) sessionId
                |> Remote.sendRequest
                |> Cmd.map SessionResponse
    in
    Model sessionId Remote.Loading Remote.NotAsked Remote.NotAsked Remote.NotAsked Nothing 1140 0 Remote.NotAsked Nothing Nothing Remote.NotAsked Remote.NotAsked ! [ loadSessionDetail, getWindowWidth ]


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
    | HowLongButtonClicked
    | HowLongCloseClicked
    | DistanceMetricLoaded (Remote.WebData DistanceMetrics)
    | MessagesLoaded (Remote.WebData MessageList)


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        SessionResponse response ->
            case response of
                Remote.Success sessionInfo ->
                    let
                        messageQuery =
                            Nexosis.Api.Messages.MessageQuery (Just sessionInfo.sessionId) (Just [ Status ]) 0 5

                        getMessages =
                            Nexosis.Api.Messages.get (contextToAuth context) messageQuery
                                |> Remote.sendRequest
                                |> Cmd.map MessagesLoaded
                    in
                    if sessionInfo.sessionId /= model.sessionId then
                        -- we got a refresh updated for a different session Id
                        model => Cmd.none
                    else if sessionInfo.status == Status.Completed then
                        let
                            details =
                                case sessionInfo.predictionDomain of
                                    PredictionDomain.Classification ->
                                        getConfusionMatrix (contextToAuth context) model.sessionId 0 25
                                            |> Remote.sendRequest
                                            |> Cmd.map ConfusionMatrixLoaded

                                    PredictionDomain.Anomalies ->
                                        getDistanceMetrics (contextToAuth context) model.sessionId 0 1000
                                            |> Remote.sendRequest
                                            |> Cmd.map DistanceMetricLoaded

                                    _ ->
                                        Ports.setPageTitle (formatDisplayName sessionInfo.name ++ " Details")
                        in
                        { model | loadingResponse = response }
                            => Cmd.batch
                                [ Nexosis.Api.Sessions.results (contextToAuth context) model.sessionId 0 1000
                                    |> Remote.sendRequest
                                    |> Cmd.map ResultsResponse
                                , details
                                , Ports.setPageTitle (formatDisplayName sessionInfo.name ++ " Details")
                                ]
                    else if not <| sessionIsCompleted sessionInfo then
                        { model | loadingResponse = response }
                            => Cmd.batch
                                [ delayAndRecheckSession context model.sessionId
                                , getMessages
                                ]
                    else
                        { model | loadingResponse = response } => Cmd.none

                Remote.Failure err ->
                    { model | loadingResponse = response } => Log.logHttpError err

                _ ->
                    { model | loadingResponse = response } => Cmd.none

        ResultsResponse response ->
            case Remote.map2 (,) response model.loadingResponse of
                Remote.Success ( results, session ) ->
                    let
                        includedColumns =
                            session.columns |> List.filter (\c -> c.role == Columns.Target || c.role == Columns.Timestamp) |> List.map (\c -> c.name)

                        timeSeriesDataRequest domain =
                            let
                                dates =
                                    getDataDateRange { startDate = session.startDate, endDate = session.endDate, resultInterval = session.resultInterval, predictionDomain = session.predictionDomain }
                            in
                            getDataByDateRange (contextToAuth context) (toDataSetName session.dataSourceName) (Just dates) includedColumns
                                |> Remote.sendRequest
                                |> Cmd.map DataSetLoaded

                        cmd =
                            case session.predictionDomain of
                                PredictionDomain.Forecast ->
                                    timeSeriesDataRequest session.predictionDomain

                                PredictionDomain.Impact ->
                                    timeSeriesDataRequest session.predictionDomain

                                _ ->
                                    Cmd.none
                    in
                    { model | resultsResponse = response, predictionDomain = Just session.predictionDomain }
                        => cmd

                Remote.Failure err ->
                    { model | resultsResponse = response } => Log.logHttpError err

                _ ->
                    { model | resultsResponse = response } => Cmd.none

        ShowDeleteDialog model ->
            { model | deleteDialogModel = Just (DeleteDialog.init "" model.sessionId) }
                => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                ignoreCascadeParams cmd _ =
                    cmd

                pendingDeleteCmd =
                    Nexosis.Api.Sessions.delete (contextToAuth context) >> ignoreCascadeParams

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
            case response of
                Remote.Success data ->
                    { model | dataSetResponse = response } => Cmd.none

                Remote.Failure err ->
                    model => Log.logHttpError err

                _ ->
                    model => Cmd.none

        MessagesLoaded response ->
            case response of
                Remote.Success data ->
                    { model | messagesResponse = response } => Cmd.none

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
                    Nexosis.Api.Sessions.resultsCsv (contextToAuth context) model.sessionId
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

        HowLongButtonClicked ->
            { model | howLongModalModel = viewHowLongSessionModal context } => Cmd.none

        HowLongCloseClicked ->
            { model | howLongModalModel = Nothing } => Cmd.none

        DistanceMetricLoaded result ->
            case result of
                Remote.Success metrics ->
                    { model | distanceMetricsResponse = result } => Cmd.none

                _ ->
                    model => Cmd.none


type alias SessionDateData =
    { startDate : Maybe String
    , endDate : Maybe String
    , resultInterval : Maybe ResultInterval
    , predictionDomain : PredictionDomain.PredictionDomain
    }


viewHowLongSessionModal : ContextModel -> Maybe (Modal.Config Msg)
viewHowLongSessionModal context =
    Just
        { closeMessage = HowLongCloseClicked
        , header = Nothing
        , body = Just (div [ class "help" ] [ explainer context.config "session_start_timing" ])
        , footer = Just (div [] [ button [ class "btn btn-danger btn-sm", onClick HowLongCloseClicked ] [ text "Dismiss" ] ])
        }


getDataDateRange : SessionDateData -> ( String, String )
getDataDateRange { startDate, endDate, resultInterval, predictionDomain } =
    case Maybe.map2 (,) startDate endDate of
        Just ( start, end ) ->
            let
                startDate =
                    DateTime.fromISO8601 start |> Result.withDefault DateTime.epoch

                endDate =
                    DateTime.fromISO8601 end |> Result.withDefault DateTime.epoch

                count =
                    predictionCount resultInterval startDate endDate
            in
            case predictionDomain of
                PredictionDomain.Forecast ->
                    ( startDate |> extendDate resultInterval (count * 2) |> DateTime.toISO8601
                    , endDate |> DateTime.toISO8601
                    )

                PredictionDomain.Impact ->
                    ( startDate |> extendDate resultInterval count |> DateTime.toISO8601
                    , endDate |> extendDate resultInterval (count * -1) |> DateTime.toISO8601
                    )

                _ ->
                    ( "", "" )

        Nothing ->
            ( "", "" )


delayAndRecheckSession : ContextModel -> String -> Cmd Msg
delayAndRecheckSession context sessionId =
    delayTask 5
        |> Task.andThen (\_ -> Nexosis.Api.Sessions.getOne (contextToAuth context) sessionId |> Http.toTask)
        |> Remote.asCmd
        |> Cmd.map SessionResponse


formatFilename : Model -> String
formatFilename model =
    model.sessionId ++ "-results." ++ Format.dataFormatToString Format.Csv


predictionCount : Maybe ResultInterval -> DateTime -> DateTime -> Int
predictionCount interval start end =
    let
        delta =
            DateTime.delta start end
    in
    case interval of
        Just Hour ->
            delta.hours

        Just Day ->
            delta.days

        Just Week ->
            delta.days // 7

        Just Month ->
            delta.months

        Just Year ->
            delta.years

        Nothing ->
            0


extendDate : Maybe ResultInterval -> (Int -> DateTime -> DateTime)
extendDate interval =
    case interval of
        Just Hour ->
            DateTime.addHours

        Just Day ->
            DateTime.addDays

        Just Week ->
            let
                extend count from =
                    DateTime.addDays (count * 7) from
            in
            extend

        Just Month ->
            DateTime.addMonths

        Just Year ->
            DateTime.addYears

        Nothing ->
            DateTime.addDays


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ id "page-header", class "row" ]
            [ Breadcrumb.detail AppRoutes.Sessions "Sessions"
            , viewSessionHeader model
            ]
        , viewSessionDetails model context
        ]


viewSessionDetails : Model -> ContextModel -> Html Msg
viewSessionDetails model context =
    let
        loadingOr =
            loadingOrView model model.loadingResponse

        pendingOrCompleted model session =
            if session.status == Status.Completed then
                div []
                    [ viewSessionDataColumn model session
                    ]
            else
                div []
                    [ viewPendingSession session ]

        sessionHistory : Model -> SessionData -> Html Msg
        sessionHistory model session =
            let
                completed =
                    session.status == Status.Completed || session.status == Status.Failed

                loadingOrMessages =
                    loadingOrView model model.messagesResponse
            in
            case completed of
                True ->
                    div []
                        [ viewMessages model session
                        , viewStatusHistory model session
                        ]

                False ->
                    div []
                        [ loadingOrMessages viewStatusMessages
                        , viewMessages model session
                        ]
    in
    div []
        [ div [ class "row", id "details" ]
            [ div [ class "col-sm-4" ]
                [ loadingOr pendingOrCompleted ]

            --, p []
            --    [ a [ class "btn btn-xs btn-primary", href "dashboard-session-champion.html" ]
            --        [ text "(TODO) View algorithm contestants" ]
            --    ]
            , div [ class "col-sm-3" ]
                [ loadingOr viewSessionInfo
                ]
            , div [ class "col-sm-5" ]
                [ loadingOr sessionHistory ]
            ]
        , viewConfusionMatrix model (loadingOr (viewAlgorithmOverview context))
        , loadingOr (viewResultsGraph (loadingOr (viewAlgorithmOverview context)))
        , viewResultsTable model
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete Session"
            , bodyMessage = Just "This action cannot be undone but you can always run another session with the same parameters."
            , associatedAssets = []
            }
            |> Html.map DeleteDialogMsg
        , Modal.view model.howLongModalModel
        ]


viewSessionInfo : Model -> SessionData -> Html Msg
viewSessionInfo model session =
    let
        s =
            session
    in
    div []
        [ modelLink session
        , p []
            [ strong []
                [ text "Source: " ]
            , a [ AppRoutes.href (AppRoutes.DataSetDetail (toDataSetName session.dataSourceName)) ]
                [ text <| formatDisplayName session.dataSourceName ]
            ]
        , viewTargetColumn session
        ]


viewMessages : Model -> SessionData -> Html Msg
viewMessages model session =
    let
        expanded =
            session.status /= Status.Completed
    in
    div []
        [ p [ attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#messages", attribute "aria-expanded" (toString expanded |> String.toLower), attribute "aria-controls" "messages" ]
            [ strong [] [ text "Messages" ]
            , i [ class "fa fa-angle-down" ] []
            ]
        , makeCollapsible "messages" expanded <| Messages.viewMessages session.messages
        ]


viewStatusMessages : Model -> MessageList -> Html Msg
viewStatusMessages model messages =
    let
        displayDate createdAt dateResult =
            case dateResult of
                Ok date ->
                    toShortTimeString (fromDateTime (getTimezoneFromDate (Just createdAt)) date)

                _ ->
                    ""

        messageEntry : Nexosis.Types.Message.Message -> Html Msg
        messageEntry msg =
            tr []
                [ td [ class "number small" ]
                    [ text <| displayDate msg.createdAt <| DateTime.fromISO8601 msg.createdAt ]
                , td [] [ text msg.content ]
                ]

        expanded =
            True
    in
    div []
        [ p [ attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#status-log", attribute "aria-expanded" (toString expanded |> String.toLower), attribute "aria-controls" "status-log" ]
            [ strong [] [ text "Progress Log" ]
            , i [ class "fa fa-angle-down" ] []
            ]
        , makeCollapsible "status-log" expanded <|
            table [ class "table table-striped" ]
                [ thead []
                    [ tr []
                        [ th [ class "per10" ]
                            [ text "Date" ]
                        , th []
                            [ text "Message" ]
                        ]
                    ]
                , tbody []
                    (List.map messageEntry messages.items)
                ]
        ]


viewStatusHistory : Model -> SessionData -> Html Msg
viewStatusHistory model session =
    let
        statusEntry status =
            tr []
                [ td [ class "number small" ]
                    [ text (toShortDateTimeString status.date) ]
                , td [ class "left" ]
                    [ statusDisplay status.status
                    ]
                ]

        expanded =
            session.status /= Status.Completed
    in
    div []
        [ p [ attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#status-log", attribute "aria-expanded" (toString expanded |> String.toLower), attribute "aria-controls" "status-log" ]
            [ strong [] [ text "Status log" ]
            , i [ class "fa fa-angle-down" ] []
            ]
        , makeCollapsible "status-log" expanded <|
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
            loadingOrView model model.loadingResponse

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
            [ div [ id "action" ]
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


viewPredictButton : Model -> SessionData -> Html Msg
viewPredictButton model session =
    if canPredictSession session then
        a [ class "btn btn-danger", AppRoutes.href (AppRoutes.ModelDetail (Maybe.withDefault "" session.modelId)) ]
            [ text "Predict" ]
    else
        div [] []


viewPendingSession : SessionData -> Html Msg
viewPendingSession session =
    let
        pct =
            toString session.approximateCompletionPercentage

        showSpinner =
            (not <| Data.Session.sessionIsCompleted session) && (session.approximateCompletionPercentage == 0)

        showProgress =
            (not <| Data.Session.sessionIsCompleted session) && (session.approximateCompletionPercentage > 0)

        progress =
            div [ class "progress-bar", attribute "role" "progressbar", attribute "aria-valuemin" "0", attribute "aria-valuemax" "100", attribute "aria-valuenow" pct, attribute "style" ("width:" ++ pct ++ "%") ]
                [ span [] [ text (pct ++ "%") ]
                ]
    in
    div []
        [ h5 [ class "mb15" ] [ text "Session Status" ]
        , h4 [ class "row" ]
            [ div [ class "col-sm-3" ] [ statusDisplay session.status ]
            , div [ class "col-sm-9" ]
                [ viewIf (\() -> span [ class "pl20" ] [ spinner ]) <| showSpinner
                , viewIf (\() -> progress) <| showProgress
                ]
            ]
        , div [ class "p15" ] [ button [ class "btn btn-xs btn-default", onClick HowLongButtonClicked ] [ text "How long will my session run?" ] ]
        ]


viewSessionDataColumn : Model -> SessionData -> Html Msg
viewSessionDataColumn model session =
    div []
        [ p []
            [ strong [] [ text "Session Type: " ]
            , text (toString session.predictionDomain)
            ]
        , p []
            [ strong [] [ text "Session ID: " ]
            , br [] []
            , copyableText session.sessionId
            ]
        , p []
            [ strong [] [ text "Created: " ]
            , text (Data.DisplayDate.toShortDateString session.requestedDate)
            ]
        , p []
            [ strong [] [ text "API Endpoint URL: " ]
            , br [] []
            , copyableText ("/sessions/" ++ session.sessionId)
            ]
        , p []
            [ deleteSessionButton model
            ]
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
                    [ text <| formatDisplayName session.name ]
                ]


viewTargetColumn : SessionData -> Html Msg
viewTargetColumn session =
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
                        , text <| formatDisplayName col
                        ]

                Nothing ->
                    div [] []

        columnName : Maybe ColumnMetadata -> Maybe String
        columnName col =
            case col of
                Nothing ->
                    Nothing

                Just c ->
                    Just <| formatDisplayName c.name
    in
    div []
        [ viewTargetColumn (targetColumn session)
        ]


viewAlgorithmOverview : ContextModel -> Model -> SessionData -> Html Msg
viewAlgorithmOverview context model session =
    let
        algorithmName : Maybe Algorithm -> String
        algorithmName algo =
            case algo of
                Nothing ->
                    ""

                Just a ->
                    a.name
    in
    div [ class "col-sm-4 pt15" ]
        [ h6 [] [ strong [] [ text "Algorithm: " ], text (algorithmName session.algorithm) ]
        , hr [] []
        , p [] [ strong [] [ text "Metrics" ] ]
        , div [] [ errorOrView model.resultsResponse <| viewMetricsList context ]
        ]


viewMetricsList : ContextModel -> SessionResults -> Html Msg
viewMetricsList context results =
    let
        listMetric key value =
            tr []
                [ th []
                    [ strong []
                        ([ text (getMetricNameFromKey context.metricExplainers key) ]
                            ++ helpIconFromText (getMetricDescriptionFromKey context.metricExplainers key)
                        )
                    ]
                , td [ class "number" ]
                    [ styledNumber <| formatFloatToString value
                    ]
                ]
    in
    table [ class "table table-striped metrics" ]
        [ tbody [] (Dict.foldr (\key val html -> listMetric key val :: html) [] results.metrics) ]


viewSessionName : Model -> SessionData -> Html Msg
viewSessionName model session =
    div [ class "col-sm-9" ]
        [ h2 [] [ text <| formatDisplayName session.name ]
        ]


viewSessionId : SessionData -> Html Msg
viewSessionId session =
    div [ class "col-sm-3" ]
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


viewResultsGraph : Html Msg -> Model -> SessionData -> Html Msg
viewResultsGraph metrics model session =
    let
        graphSpec =
            graphModel model
    in
    case graphSpec of
        Just spec ->
            div []
                [ div [ class "col-sm-12" ]
                    [ div [ class "col-sm-4" ] []
                    , div [ class "col-sm-8 action" ] [ renderExplanatoryButton session ]
                    ]
                , div []
                    [ div [ class "col-sm-8" ]
                        [ div [ class "center" ] [ spec ] ]
                    , metrics
                    ]
                ]

        Nothing ->
            div [ class "col-sm-8" ]
                [ div [] [] ]


graphModel : Model -> Maybe (Html msg)
graphModel model =
    case ( model.loadingResponse, model.resultsResponse, model.dataSetResponse, model.distanceMetricsResponse ) of
        ( Remote.Success session, Remote.Success results, Remote.Success data, _ ) ->
            case session.predictionDomain of
                PredictionDomain.Forecast ->
                    Charts.forecastResults results session data model.windowWidth |> Just

                PredictionDomain.Impact ->
                    Charts.impactResults results session data model.windowWidth |> Just

                _ ->
                    Nothing

        ( Remote.Success session, Remote.Success results, _, Remote.Success metrics ) ->
            case session.predictionDomain of
                PredictionDomain.Anomalies ->
                    Charts.anomalyResults results session metrics model.windowWidth |> Just

                _ ->
                    Nothing

        ( Remote.Success session, Remote.Success results, _, _ ) ->
            case session.predictionDomain of
                PredictionDomain.Regression ->
                    Charts.regressionResults results session model.windowWidth |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


viewResultsTable : Model -> Html Msg
viewResultsTable model =
    case model.loadingResponse of
        Remote.Success sessionResponse ->
            if sessionResponse.status == Status.Completed then
                if
                    (sessionResponse.predictionDomain == PredictionDomain.Forecast)
                        || (sessionResponse.predictionDomain == PredictionDomain.Impact)
                then
                    viewTimeSeriesResults model sessionResponse
                else if sessionResponse.predictionDomain == PredictionDomain.Anomalies then
                    viewAnomalyResults model sessionResponse
                else
                    viewModelTrainingResults model sessionResponse
            else
                div [] []

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
                    [ div [ class "row mb15" ]
                        [ div [ class "col-sm-4 p0" ] [ h3 [] [ text "Test Data" ] ]
                        , div [ class "col-sm-8 action" ] [ button [ class "btn btn-danger", onClick DownloadResults ] [ text "Download Results" ] ]
                        ]
                    , table [ class "table table-striped" ]
                        [ thead []
                            [ tr []
                                [ th [ class "left" ] [ text <| formatDisplayName target.name ++ " - Actual" ]
                                , th [ class "left" ] [ text <| formatDisplayName target.name ++ " - Predicted" ]
                                ]
                            ]
                        , tbody [] (List.map renderRow (filterToPage pagedData))
                        ]
                    , div [ class "center" ] [ Pager.view pagedData ChangePage ]
                    ]
                ]

        Nothing ->
            div [] []


renderExplanatoryButton : SessionData -> Html Msg
renderExplanatoryButton session =
    if session.predictionDomain == PredictionDomain.Regression || session.predictionDomain == PredictionDomain.Anomalies then
        div [ style [ ( "margin-top", "5px" ) ], id "action" ]
            [ a [ href ("https://docs.nexosis.com/guides/analyzing-" ++ (String.toLower <| toString session.predictionDomain) ++ "-results"), target "_blank" ]
                [ button [ class "btn btn-default btn-sm" ] [ Html.text "Understanding your results" ]
                ]
            ]
    else
        span [] []


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
            [ div [ class "row mb15" ]
                [ div [ class "col-xs-4" ] [ h3 [] [ text "Anomaly Results" ] ]
                , div [ class "col-xs-8 action" ] [ button [ class "btn btn-danger", onClick DownloadResults ] [ text "Download Results" ] ]
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


viewTimeSeriesResults : Model -> SessionData -> Html Msg
viewTimeSeriesResults model sessionData =
    let
        timeZoneOffset =
            getTimezoneFromDate sessionData.startDate

        pagedData =
            Remote.map (.data >> mapToPagedListing model.currentPage) model.resultsResponse

        targetColumn =
            sessionData.columns
                |> List.find (\c -> c.role == Columns.Target)

        timestampColumn =
            sessionData.columns
                |> List.find (\c -> c.role == Columns.Timestamp)
    in
    case Maybe.map2 (,) targetColumn timestampColumn of
        Just ( target, timestamp ) ->
            let
                renderRow datum =
                    let
                        time =
                            Dict.get timestamp.name datum
                                |> formatDateWithTimezone timeZoneOffset

                        predicted =
                            Dict.get target.name datum
                    in
                    tr []
                        [ td [ class "left number" ] [ viewJust (\a -> text a) time ]
                        , td [ class "left number" ] [ viewJust (\p -> text p) predicted ]
                        ]
            in
            div [ class "row" ]
                [ div [ class "col-sm-12" ]
                    [ div [ class "row" ]
                        [ div [ class "col-sm-9" ] [ h3 [] [ text "Results" ] ]
                        , div [ class "col-sm-3" ] [ div [ class "mt5 right" ] [ button [ class "btn btn-danger", onClick DownloadResults ] [ text "Download Results" ] ] ]
                        ]
                    , table [ class "table table-striped" ]
                        [ thead []
                            [ tr []
                                [ th [ class "left" ] [ text "Timestamp" ]
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


loadingOrView : Model -> Remote.WebData a -> (Model -> a -> Html Msg) -> Html Msg
loadingOrView model request view =
    case request of
        Remote.Success resp ->
            view model resp

        Remote.Loading ->
            div [ class "loading--line" ] []

        _ ->
            div [] []


viewConfusionMatrix : Model -> Html Msg -> Html Msg
viewConfusionMatrix model metrics =
    case model.confusionMatrixResponse of
        Remote.NotAsked ->
            div [] []

        Remote.Loading ->
            div [] []

        Remote.Success response ->
            div [] [ Charts.renderConfusionMatrix response metrics, hr [] [] ]

        Remote.Failure error ->
            viewHttpError error
