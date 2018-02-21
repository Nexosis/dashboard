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
import Data.PredictionDomain exposing (..)
import Data.Columns exposing (ColumnMetadata, Role)
import List.Extra exposing (find)

type alias Model =
    { 
       modelId : String 
       , modelResponse : Remote.WebData ModelData
       , config : Config
       , modelType : PredictionDomain
    }

init : Config -> String -> ( Model, Cmd Msg )
init config modelId =
    let
        loadModelDetail =
            Request.Model.getOne config modelId
                |> Remote.sendRequest
                |> Cmd.map ModelResponse
    in
    Model modelId Remote.Loading config Regression => loadModelDetail

type Msg
    = ModelResponse (Remote.WebData ModelData)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ModelResponse response->
            case response of
                Remote.Success modelInfo -> 
                    {model | modelResponse = response, modelType = modelInfo.predictionDomain} => Cmd.none
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
                , div [class "row"]
                [ 
                    dataSourceName model
                    ,div [class "col-sm-3"][
                        div [class "mt10 right"][
                            button [class "btn", href "#/predict"][text "Predict"]
                        ]
                    ]
                ]
                , div [class "row"][
                    div [class "col-sm-4"][p[class "small"][
                        strong [][text "Model ID:"]
                        ,text (padSpace model.modelId)
                        ,a [][i [class "fa fa-copy color-mediumGray"][]]
                    ]]
                    ,div [class "col-sm-4"][p[class "small"][
                        strong [][text "Model Type:"]
                        ,text (padSpace (toString model.modelType))
                    ]]
                    ,div [class "col-sm-4 right"][button[class "btn btn-xs secondary"][
                        i [class "fa fa-trash-o mr5"][]
                        ,text "Delete"
                    ]]
                ]
                , div [class "row"][
                    div [class "col-sm-4"][
                        detailRow model
                    ]
                    ,div [class "col-sm-5"][]
                    ,div [class "col-sm-3"][]
                ]
            ]
        

dataSourceName : Model -> Html Msg
dataSourceName model = 
        case model.modelResponse of
        Remote.Success resp ->
            div[class "col-sm-9"][h2[class "mt10"][text ("Model for " ++ resp.dataSourceName)]]
        Remote.Loading ->
            text "Loading..."
        _ -> 
            text "Not found" 

detailRow : Model -> Html Msg
detailRow model = 
    case model.modelResponse of
        Remote.Success resp ->
           div[][
               h5[class "mt15 mb15"][text "Details"]
               ,p[][
                   strong[][text "Session Used:"]
                   ,a[href "#"][text resp.sessionId]
               ]
               ,p[][
                   strong[][text "Source:"]
                   ,a[href "#"][text resp.dataSourceName]
               ]
               ,p[][
                   strong[][text "Target Column:"]
                   ,text (find (\c -> (c.role == Data.Columns.Target)) resp.columns |> Maybe.map (\t -> t.name) |> Maybe.withDefault "")
               ]
               ,p[][
                   strong[][text "Algorithm:"]
                   ,text resp.algorithm.name
               ]
           ]
        Remote.Loading ->
            text "Loading..."
        _ -> 
            text "Not found" 

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

padSpace : String -> String
padSpace input = 
    " " ++ input ++ " "