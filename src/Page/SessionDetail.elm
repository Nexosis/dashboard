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


viewSessionName : SessionData -> Html Msg
viewSessionName resp = 
    div [class "col-sm-9"]
        [h2 [class "mt10"][text resp.name]
    ]
    

viewSessionId : SessionData -> Html Msg
viewSessionId resp = 
    div [ class "col-sm-4" ]
        [ p [ class "small" ]
            [ strong []
                [ text "Session ID:" ]
            , text resp.sessionId
            , a []
                [ i [ class "fa fa-copy color-mediumGray" ]
                    []
                ]
            ]
        ]

init : Config -> String -> ( Model, Cmd Msg )
init config sessionId =
    let
        loadModelDetail =
            Request.Session.getOne config sessionId
                |> Remote.sendRequest
                |> Cmd.map SessionResponse
    in
    Model sessionId Remote.Loading config => loadModelDetail
