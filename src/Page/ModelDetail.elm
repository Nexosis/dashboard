module Page.ModelDetail exposing (Model, Msg, update, view, init)

import Util exposing ((=>))
import Html exposing (..)
import Request.Model exposing (getOne)
import Data.Config
import RemoteData as Remote
import Data.Config exposing (Config)
import Data.Model exposing (..)
import Request.Log as Log
import Html.Attributes exposing (..)

type alias Model =
    { 
       modelId : String 
       , modelResponse : Remote.WebData ModelData
       , config : Config
    }

init : Config -> String -> ( Model, Cmd Msg )
init config modelId =
    let
        loadModelDetail =
            Request.Model.getOne config modelId
                |> Remote.sendRequest
                |> Cmd.map ModelResponse
    in
    Model "" Remote.Loading config => loadModelDetail

type Msg
    = ModelResponse (Remote.WebData ModelData)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ModelResponse response->
            case response of
                Remote.Success modelInfo -> 
                    {model | modelResponse = response} => Cmd.none
                Remote.Failure err ->
                    model => (Log.logMessage <| Log.LogMessage ("Model details response failure: " ++ toString err) Log.Error)
                _ ->
                    model => Cmd.none

view : Model -> Html Msg
view model =
        div [][
                p [class "breadcrumb"][
                    span[][
                        a[href "#"][text "Api Dashboard"]
                    ]
                    ,i [class "fa fa-angle-right", style [("margin","0 5px")]][]
                    ,span[][a[href "/#/models"][text "Models"]]
                ]
                ,dataSourceName model
            ]
        

dataSourceName : Model -> Html Msg
dataSourceName model = 
    div [class "row"]
        [
            case model.modelResponse of
            Remote.Success resp ->
                div[class "col-sm-9"][h2[class "mt10"][text resp.dataSourceName]]
            Remote.Loading ->
                text "Loading..."
            _ -> 
                text "Not found"
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
