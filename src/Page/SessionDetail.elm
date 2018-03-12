module Page.SessionDetail exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Algorithm exposing (..)
import Data.Columns as Role exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.ConfusionMatrix exposing (ConfusionMatrix)
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
import List.Extra as ListX
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
import View.Messages as Messages
import Window


type alias Model =
    { sessionId : String
    , sessionResponse : Remote.WebData SessionData
    , resultsResponse : Remote.WebData SessionResults
    , confusionMatrixResponse : Remote.WebData ConfusionMatrix
    , dataSetResponse : Remote.WebData DataSetData
    , config : Config
    , deleteDialogModel : Maybe DeleteDialog.Model
    , windowWidth : Int
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
    Model sessionId Remote.Loading Remote.NotAsked Remote.NotAsked Remote.NotAsked config Nothing 1140 ! [ loadModelDetail, getWindowWidth ]


type Msg
    = SessionResponse (Remote.WebData SessionData)
    | ResultsResponse (Remote.WebData SessionResults)
    | ShowDeleteDialog Model
    | DeleteDialogMsg DeleteDialog.Msg
    | ConfusionMatrixLoaded (Remote.WebData ConfusionMatrix)
    | DataSetLoaded (Remote.WebData DataSetData)
    | GetWindowWidth (Result String Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionResponse response ->
            case response of
                Remote.Success sessionInfo ->
                    if sessionInfo.status == Status.Completed then
                        let
                            details =
                                case sessionInfo.predictionDomain of
                                    PredictionDomain.Classification ->
                                        getConfusionMatrix model.config model.sessionId 0 25
                                            |> Remote.sendRequest
                                            |> Cmd.map ConfusionMatrixLoaded

                                    _ ->
                                        Cmd.none
                        in
                        { model | sessionResponse = response, sessionId = sessionInfo.sessionId }
                            => Cmd.batch
                                [ Request.Session.results model.config model.sessionId 0 1000
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
                        cmd =
                            case session.predictionDomain of
                                PredictionDomain.Forecast ->
                                    "result-vis"
                                        => Charts.forecastResults results session model.windowWidth
                                        |> List.singleton
                                        |> Json.Encode.object
                                        |> Ports.drawVegaChart

                                PredictionDomain.Regression ->
                                    "result-vis"
                                        => Charts.regressionResults results session model.windowWidth
                                        |> List.singleton
                                        |> Json.Encode.object
                                        |> Ports.drawVegaChart

                                PredictionDomain.Impact ->
                                    let
                                        dates =
                                            Just ( Maybe.withDefault "" session.startDate, Maybe.withDefault "" session.endDate )
                                    in
                                    getDataByDateRange model.config (toDataSetName session.dataSourceName) dates
                                        |> Remote.sendRequest
                                        |> Cmd.map DataSetLoaded

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
                    Request.Session.delete model.config >> ignoreCascadeParams

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


view : Model -> Html Msg
view model =
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
            ]
        , viewSessionHeader model
        , hr [] []
        , viewSessionDetails model
        , hr [] []
        , viewConfusionMatrix model
        , viewResultsGraph model
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

        statusHistoryOrMessages session =
            if sessionIsCompleted session then
                viewMessages session
            else
                viewStatusHistory session
    in
    div [ class "row" ]
        [ div [ class "col-sm-4" ]
            [ loadingOr (pendingOrCompleted model) ]

        --, p []
        --    [ a [ class "btn btn-xs btn-primary", href "dashboard-session-champion.html" ]
        --        [ text "(TODO) View algorithm contestants" ]
        --    ]
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
    Messages.viewMessages session.messages


viewStatusHistory : SessionData -> Html Msg
viewStatusHistory session =
    let
        statusEntry status =
            tr []
                [ td [ class "small" ]
                    [ text (toShortDateTimeString status.date) ]
                , td [ class "left" ]
                    [ statusDisplay status.status
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

        disabledOr : Remote.WebData a -> (Maybe a -> Bool -> Html Msg) -> Html Msg
        disabledOr request view =
            case request of
                Remote.Success resp ->
                    view (Just resp) False

                _ ->
                    view Nothing True
    in
    div []
        [ div [ class "row" ]
            [ loadingOr viewSessionName
            , div [ class "col-sm-3" ]
                [ div [ class "mt5 right" ]
                    [ div
                        [ class "btn-group", attribute "role" "group" ]
                        [ loadingOr iterateSessionButton
                        , loadingOr viewPredictButton
                        ]
                    ]
                ]

            {- , div [ class "row" ]
               [ loadingOr viewSessionId
               , div [ class "col-sm-4" ]
                   [ p [ class "small" ]
                       [ strong []
                           [ text "Session Type: " ]
                       , loadingOr (\s -> text <| toString s.predictionDomain)
                       ]
                   ]
               , div [ class "col-sm-4 right" ]
                   [ viewSessionButtons model
                   ]
            -}
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
        , text "Delete"
        ]


viewPredictButton : SessionData -> Html Msg
viewPredictButton session =
    if canPredictSession session then
        a [ class "btn btn-danger", AppRoutes.href (AppRoutes.ModelDetail (Maybe.withDefault "" session.modelId) True) ]
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
                , a [ AppRoutes.href (AppRoutes.ModelDetail modelId False) ]
                    [ text session.name ]
                ]


viewCompletedSession : SessionData -> Html Msg
viewCompletedSession session =
    let
        targetColumnFromColumns : SessionData -> Maybe String
        targetColumnFromColumns session =
            session.columns
                |> ListX.find (\m -> m.role == Role.Target)
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

        Remote.Failure _ ->
            div [] [ text "Error" ]
