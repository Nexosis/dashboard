module Page.ModelDetail exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes as Routes
import Data.Columns exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.Context exposing (ContextModel)
import Data.DataSet exposing (toDataSetName)
import Data.Model exposing (..)
import Data.PredictionDomain exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (find)
import Page.ModelPredict as ModelPredict
import Ports
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (getOne)
import Util exposing ((=>), formatFloatToString, styledNumber)
import View.Breadcrumb as Breadcrumb
import View.CopyableText exposing (copyableText)
import View.DeleteDialog as DeleteDialog


type alias Model =
    { modelId : String
    , modelResponse : Remote.WebData ModelData
    , modelType : PredictionDomain
    , deleteDialogModel : Maybe DeleteDialog.Model
    , predictModel : Maybe ModelPredict.Model
    }


init : Config -> String -> ( Model, Cmd Msg )
init config modelId =
    let
        loadModelDetail =
            Request.Model.getOne config modelId
                |> Remote.sendRequest
                |> Cmd.map ModelResponse
    in
    Model modelId Remote.Loading Regression Nothing Nothing => loadModelDetail


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.predictModel of
        Just predictModel ->
            ModelPredict.subscriptions predictModel
                |> Sub.map ModelPredictMsg

        Nothing ->
            Sub.none


type Msg
    = ModelResponse (Remote.WebData ModelData)
    | ModelPredictMsg ModelPredict.Msg
    | ShowDeleteDialog
    | DeleteDialogMsg DeleteDialog.Msg


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        ModelResponse response ->
            case response of
                Remote.Success modelInfo ->
                    { model | modelResponse = response, modelType = modelInfo.predictionDomain, predictModel = Just (ModelPredict.init context.config model.modelId) } => Ports.setPageTitle (Maybe.withDefault "Model" modelInfo.modelName ++ " Details")

                Remote.Failure err ->
                    model => (Log.logMessage <| Log.LogMessage ("Model details response failure: " ++ toString err) Log.Error)

                _ ->
                    model => Cmd.none

        ModelPredictMsg subMsg ->
            let
                ( predictModel, cmd ) =
                    case model.predictModel of
                        Just predictModel ->
                            ModelPredict.update subMsg predictModel context |> Tuple.mapFirst Just

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
            { model | predictModel = predictModel } => Cmd.map ModelPredictMsg cmd

        ShowDeleteDialog ->
            let
                modelName =
                    "model " ++ model.modelId
            in
            { model | deleteDialogModel = Just (DeleteDialog.init modelName model.modelId) } => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                ignoreCascadeParams request _ =
                    request

                pendingDeleteCmd =
                    Request.Model.delete context.config >> ignoreCascadeParams

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            Routes.modifyUrl Routes.Models
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ id "page-header", class "row" ]
            [ Breadcrumb.detail Routes.Models "Models"
            , modelName model
            , div [ class "col-sm-3" ]
                []
            ]
        , detailRow model
        , hr [] []
        , renderPredict context model
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete Model"
            , bodyMessage = Just "This action cannot be undone. You will have to run another session to replace this model."
            , associatedAssets = []
            }
            |> Html.map DeleteDialogMsg
        ]


renderPredict : ContextModel -> Model -> Html Msg
renderPredict context model =
    case model.predictModel of
        Just predictModel ->
            ModelPredict.view predictModel context |> Html.map ModelPredictMsg

        Nothing ->
            div [] []


modelName : Model -> Html Msg
modelName model =
    case model.modelResponse of
        Remote.Success resp ->
            div [ class "col-sm-9" ] [ h2 [ class "mt10" ] [ text (Maybe.withDefault ("Model For " ++ resp.dataSourceName) resp.modelName) ] ]

        Remote.Loading ->
            text "Loading..."

        _ ->
            text "Not found"


detailRow : Model -> Html Msg
detailRow model =
    case model.modelResponse of
        Remote.Success resp ->
            div [ class "row" ]
                [ div [ class "col-sm-12" ] [ h5 [ class "mt15 mb15" ] [ text "Details" ] ]
                , div [ class "col-sm-4" ]
                    [ p []
                        [ strong [] [ text "Session Used: " ]
                        , a [ Routes.href (Routes.SessionDetail resp.sessionId) ] [ text resp.sessionId ]
                        ]
                    , p []
                        [ strong [] [ text "Source: " ]
                        , a [ Routes.href (Routes.DataSetDetail (toDataSetName resp.dataSourceName)) ] [ text resp.dataSourceName ]
                        ]
                    , p []
                        [ strong [] [ text "Target Column: " ]
                        , text (find (\c -> c.role == Data.Columns.Target) resp.columns |> Maybe.map (\t -> t.name) |> Maybe.withDefault "")
                        ]
                    ]
                , div [ class "col-sm-4" ] [ metricsList resp.algorithm.name resp.metrics ]
                , div [ class "col-sm-4 " ]
                    [ p [] [ strong [] [ text "Model Type: " ], text (toString model.modelType) ]
                    , p []
                        [ strong [] [ text "Model ID:" ]
                        , br [] []
                        , copyableText model.modelId
                        ]
                    , p []
                        [ strong [] [ text "API Endpoint Url" ]
                        , br [] []
                        , copyableText ("/models/" ++ model.modelId)
                        ]
                    , button [ class "btn btn-xs btn-primary", onClick ShowDeleteDialog ]
                        [ i [ class "fa fa-trash-o mr5" ] []
                        , text "Delete Model"
                        ]
                    ]
                ]

        Remote.Loading ->
            text "Loading..."

        _ ->
            text "Not found"


metricsList : String -> Dict String Float -> Html Msg
metricsList algo metrics =
    div []
        [ p []
            [ strong [] [ text "Algorithm: " ]
            , text algo
            ]
        , p [ class "small", attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#metrics", attribute "aria-expanded" "true", attribute "aria-controls" "metrics" ]
            [ strong [] [ text "Metrics" ]
            , i [ class "fa fa-angle-down ml5" ] []
            ]
        , ul [ class "small algorithm-metrics collapse", id "metrics" ] (List.map metricListItem (Dict.toList metrics))
        ]


metricListItem : ( String, Float ) -> Html Msg
metricListItem ( name, value ) =
    li []
        [ strong [] [ text name ]
        , br [] []
        , styledNumber <| formatFloatToString value
        ]


padSpace : String -> String
padSpace input =
    " " ++ input ++ " "
