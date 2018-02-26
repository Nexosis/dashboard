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
        ]
    ]

viewSessionName : Model -> Html Msg
viewSessionName model = 
    case model.sessionResponse of
        Remote.Success resp ->
            div [class "col-sm-9"]
                [h2 [class "mt10"][text resp.name]
            ]
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
