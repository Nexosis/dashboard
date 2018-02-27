module Page.SessionDetail exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Data.Session exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (getOne)
import Request.Session exposing (..)
import Util exposing ((=>))
import AppRoutes
import List exposing (head, filter)
import Data.Columns exposing (ColumnMetadata)
import Data.Columns as Role exposing (Role)

type alias Model =
    { sessionId : String
    , sessionResponse : Remote.WebData SessionData
    , config : Config
    }


type Msg
    = SessionResponse (Remote.WebData SessionData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionResponse response ->
            case response of
                Remote.Success sessionInfo ->
                    { model | sessionResponse = response, sessionId = sessionInfo.sessionId } => Cmd.none

                Remote.Failure err ->
                    model => (Log.logMessage <| Log.LogMessage ("Session details response failure: " ++ toString err) Log.Error)

                _ ->
                    model => Cmd.none


view : Model -> Html Msg
view model =
    div[]
    [
        p [ class "breadcrumb" ]
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
        , viewSessionDetails model
    ]

viewSessionDetails : Model -> Html Msg
viewSessionDetails model = 
    div [class "row"]
        [
            
        ]


viewSessionHeader : Model -> Html Msg
viewSessionHeader model = 
    let 
        loadingOr = loadingOrView model.sessionResponse
    in
        div[]
        [
        div [class "row"]
            [ loadingOr viewSessionName
            ,div [ class "col-sm-3" ]
                [ div [ class "mt10 right" ]
                    [ button [ class "btn" ]
                        [ text "Predict" ]
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
                        , text "Iterate session"
                        ]
                    , button [ class "btn btn-xs secondary" ]
                        [ i [ class "fa fa-trash-o mr5" ]
                            []
                        , text "Delete"
                        ]
                    ]
                ]
            , div [class "row"]
                [
                    div [class "col-sm-4"]
                        [
                            loadingOr viewSessionDetail
                        ]
                    , div [class "col-sm-4"]
                        [
                            
                        ]
                    , div [class "col-sm-4"]
                        [
                            
                        ]
                ]
        ]


viewSessionDetail : SessionData -> Html Msg
viewSessionDetail session = 
    if session.status == Completed then
        viewCompletedSession session
    else
        viewPendingSession session
        


viewPendingSession : SessionData -> Html Msg
viewPendingSession session = 
    h5 [ class "mb15" ]
    [ text "Session Status" ]

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
                Nothing -> ""
                Just c -> c.name
            

    in
        div []
        [ h5 [ class "mt15 mb15" ]
            [ text "Details" ]
        , p []
            [ strong []
                [ text "Model: " ]
            , a [ href "#" ]
                [ text "{ModelName}" ]
            ]
        , p []
            [ strong []
                [ text "Source: " ]
            , a [ href "#" ]
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
            , text "{AlgorithmName}"
            ]
        , p [ class "small" ]
            [ strong []
                [ text "Metrics" ]
            ]
        , ul [ class "small algorithm-metrics" ]
            [ li []
                [ strong []
                    [ text "meanAbsoluteError:" ]
                , br []
                    []
                , text "1715319610074.247"
                ]
            , li []
                [ strong []
                    [ text "meanAbsolutePercentError:" ]
                , br []
                    []
                , text "150408208493.396"
                ]
            , li []
                [ strong []
                    [ text "rootMeanSquareError:" ]
                , br []
                    []
                , text "10855736798156.244"
                ]
            ]
        , p []
            [ a [ class "btn btn-xs secondary", href "dashboard-session-champion.html" ]
                [ text "View algorithm contestants" ]
            ]
        ]

viewSessionName : SessionData -> Html Msg
viewSessionName session = 
    div [class "col-sm-9"]
        [h2 [class "mt10"][text session.name]
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

loadingOrView : Remote.WebData SessionData -> (SessionData -> Html Msg) -> Html Msg
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
    Model sessionId Remote.Loading config => loadModelDetail
