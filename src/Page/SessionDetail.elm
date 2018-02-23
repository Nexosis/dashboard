module Page.SessionDetail exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Data.Session exposing (..)
import Html exposing (..)
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (getOne)
import Request.Session exposing (..)
import Util exposing ((=>))


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
    div []
        [ h2 [] [ text ("This is detail for session " ++ model.sessionId) ]
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
