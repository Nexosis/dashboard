module Page.SessionDetail exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Algorithm exposing (..)
import Data.Columns as Role exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.DataSet exposing (toDataSetName)
import Data.DisplayDate exposing (toShortDateTimeString)
import Data.Message exposing (..)
import Data.Session exposing (..)
import Data.Status exposing (Status)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (filter, foldr, head)
import List.Extra as ListX
import Page.Helpers exposing (..)
import RemoteData as Remote
import Request.Log as Log
import Request.Session exposing (..)
import Util exposing ((=>),formatFloatToString)
import View.DeleteDialog as DeleteDialog


type alias Model =
    { sessionId : String
    , sessionResponse : Remote.WebData SessionData
    , resultsResponse : Remote.WebData SessionResults
    , config : Config
    , deleteDialogModel : Maybe DeleteDialog.Model
    }


type Msg
    = SessionResponse (Remote.WebData SessionData)
    | ResultsResponse (Remote.WebData SessionResults)
    | ShowDeleteDialog Model
    | DeleteDialogMsg DeleteDialog.Msg


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
            if session.status == Data.Status.Completed then
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
        --    [ a [ class "btn btn-xs secondary", href "dashboard-session-champion.html" ]
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
                [ viewSessionButtons model
                ]
            ]
        ]


viewSessionButtons : Model -> Html Msg
viewSessionButtons model =
    div []
        [ button [ class "btn btn-xs other" ]
            [ i [ class "fa fa-repeat mr5" ]
                []
            , text "(TODO) Iterate session"
            ]
        , button [ class "btn btn-xs secondary", onClick (ShowDeleteDialog model) ]
            [ i [ class "fa fa-trash-o mr5" ]
                []
            , text "Delete"
            ]
        ]


viewPredictButton : SessionData -> Html Msg
viewPredictButton session =
    if canPredictSession session then
        a [ class "btn", AppRoutes.href (AppRoutes.ModelDetail (Maybe.withDefault "" session.modelId) True) ]
            [ text "Predict" ]
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
        [ h5 [ class "mt15 mb15" ]
            [ text "Details" ]
        , modelLink session
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
                , text value
                ]
    in
    div []
        [ p [ class "small" ]
            [ strong []
                [ text "Metrics" ]
            ]
        , ul [ class "small algorithm-metrics" ]
            (Dict.foldr (\key val html -> listMetric key (formatFloatToString val) :: html) [] results.metrics)
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
    Model sessionId Remote.Loading Remote.NotAsked config Nothing => loadModelDetail
