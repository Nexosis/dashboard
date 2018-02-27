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
        , div [class "row"]
        [ viewSessionName model
        ,div [ class "col-sm-3" ]
            [ div [ class "mt10 right" ]
                [ button [ class "btn" ]
                    [ text "Predict" ]
                ]
            ]
        ]
        , div [ class "row" ]
            [ viewSessionId model
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
loadingOr : Remote.WebData SessionData -> (SessionData -> Html Msg) -> Html Msg
loadingOr request view =
    case request of
        Remote.Success resp ->
            view resp

        Remote.Loading ->
            div [ class "loading--line" ] [] 
        _ ->
            div [] []

viewSessionName : Model -> Html Msg
viewSessionName model = 
    let 
        name resp =
            div [class "col-sm-9"]
                [h2 [class "mt10"][text resp.name]
            ]
    in
        loadingOr model.sessionResponse name
    

viewSessionId model = 
    let
        id resp = div [ class "col-sm-4" ]
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
    in
        loadingOr model.sessionResponse id

init : Config -> String -> ( Model, Cmd Msg )
init config sessionId =
    let
        loadModelDetail =
            Request.Session.getOne config sessionId
                |> Remote.sendRequest
                |> Cmd.map SessionResponse
    in
    Model sessionId Remote.Loading config => loadModelDetail
