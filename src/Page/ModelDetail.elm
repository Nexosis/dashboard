module Page.ModelDetail exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes as Routes
import Data.Context exposing (ContextModel, contextToAuth)
import Data.Metric exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (find)
import Nexosis.Api.Models exposing (getOne)
import Nexosis.Types.Columns exposing (ColumnMetadata, Role(..))
import Nexosis.Types.DataSet exposing (toDataSetName)
import Nexosis.Types.Model exposing (..)
import Nexosis.Types.PredictionDomain exposing (..)
import Page.ModelPredict as ModelPredict
import Ports
import RemoteData as Remote
import Request.Log as Log
import Util exposing ((=>), formatDisplayName, formatFloatToString, styledNumber)
import View.Breadcrumb as Breadcrumb
import View.CopyableText exposing (copyableText)
import View.DeleteDialog as DeleteDialog
import View.Tooltip exposing (helpIconFromText)


type alias Model =
    { modelId : String
    , loadingResponse : Remote.WebData ModelData
    , modelType : PredictionDomain
    , deleteDialogModel : Maybe DeleteDialog.Model
    , predictModel : Maybe ModelPredict.Model
    }


init : ContextModel -> String -> ( Model, Cmd Msg )
init context modelId =
    let
        loadModelDetail =
            getOne (contextToAuth context) modelId
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
                    { model
                        | loadingResponse = response
                        , modelType = modelInfo.predictionDomain
                        , predictModel = Just (ModelPredict.init context.config model.modelId modelInfo.modelName)
                    }
                        => Ports.setPageTitle (Maybe.withDefault "Model" modelInfo.modelName ++ " Details")

                Remote.Failure err ->
                    { model | loadingResponse = response } => Log.logHttpError err

                _ ->
                    { model | loadingResponse = response } => Cmd.none

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
                    Nexosis.Api.Models.delete (contextToAuth context) >> ignoreCascadeParams

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
            , div [ class "col-sm-3" ] []
            ]
        , detailRow model context
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
    case model.loadingResponse of
        Remote.Success resp ->
            div [ class "col-sm-9" ] [ h2 [] [ text (Maybe.withDefault ("Model For " ++ resp.dataSourceName) resp.modelName) ] ]

        Remote.Loading ->
            text "Loading..."

        _ ->
            text "Not found"


detailRow : Model -> ContextModel -> Html Msg
detailRow model context =
    case model.loadingResponse of
        Remote.Success resp ->
            div [ id "details", class "row" ]
                [ div [ class "col-sm-4" ]
                    [ p []
                        [ strong [] [ text "Session Used: " ]
                        , a [ Routes.href (Routes.SessionDetail resp.sessionId) ] [ text resp.sessionId ]
                        ]
                    , p []
                        [ strong [] [ text "Source: " ]
                        , a [ Routes.href (Routes.DataSetDetail (toDataSetName resp.dataSourceName)) ] [ text <| formatDisplayName resp.dataSourceName ]
                        ]
                    , p []
                        [ strong [] [ text "Target Column: " ]
                        , text (find (\c -> c.role == Target) resp.columns |> Maybe.map (\t -> t.name) |> Maybe.withDefault "" |> formatDisplayName)
                        ]
                    ]
                , div [ class "col-sm-4" ] [ metricsList context resp.algorithm.name resp.metrics ]
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


metricsList : ContextModel -> String -> Dict String Float -> Html Msg
metricsList context algo metrics =
    div []
        [ p []
            [ strong [] [ text "Algorithm: " ]
            , text algo
            ]
        , p [ class "small", attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" "#metrics", attribute "aria-expanded" "true", attribute "aria-controls" "metrics" ]
            [ strong [] [ text "Metrics" ]
            , i [ class "fa fa-angle-down ml5" ] []
            ]
        , ul [ class "small algorithm-metrics collapse", id "metrics" ] (List.map (metricListItem context) (Dict.toList metrics))
        ]


metricListItem : ContextModel -> ( String, Float ) -> Html Msg
metricListItem context ( name, value ) =
    li []
        [ strong [] ([ text (getMetricNameFromKey context.metricExplainers name) ] ++ helpIconFromText (getMetricDescriptionFromKey context.metricExplainers name))
        , br [] []
        , styledNumber <| formatFloatToString value
        ]


padSpace : String -> String
padSpace input =
    " " ++ input ++ " "
