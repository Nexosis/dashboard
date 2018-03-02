module Page.ModelDetail exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes as Routes
import Data.Columns exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.DataSet exposing (toDataSetName)
import Data.Model exposing (..)
import Data.PredictionDomain exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (find)
import Page.ModelPredict as ModelPredict
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (getOne)
import Task
import Util exposing ((=>), formatFloatToString)
import View.DeleteDialog as DeleteDialog
import View.RelatedLinks as Related exposing (view)


type alias Model =
    { modelId : String
    , modelResponse : Remote.WebData ModelData
    , config : Config
    , modelType : PredictionDomain
    , deleteDialogModel : Maybe DeleteDialog.Model
    , predictModel : Maybe ModelPredict.Model
    }


init : Config -> String -> Bool -> ( Model, Cmd Msg )
init config modelId showPredict =
    let
        loadModelDetail =
            Request.Model.getOne config modelId
                |> Remote.sendRequest
                |> Cmd.map (ModelResponse showPredict)
    in
    Model modelId Remote.Loading config Regression Nothing Nothing => loadModelDetail


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.predictModel of
        Just predictModel ->
            ModelPredict.subscriptions predictModel
                |> Sub.map ModelPredictMsg

        Nothing ->
            Sub.none


type Msg
    = ModelResponse Bool (Remote.WebData ModelData)
    | TogglePredict ()
    | ModelPredictMsg ModelPredict.Msg
    | ShowDeleteDialog
    | DeleteDialogMsg DeleteDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelResponse showPredict response ->
            case response of
                Remote.Success modelInfo ->
                    let
                        getValue =
                            ()

                        nextCommand =
                            if showPredict then
                                Task.perform TogglePredict (Task.succeed ())
                            else
                                Cmd.none
                    in
                    { model | modelResponse = response, modelType = modelInfo.predictionDomain } => nextCommand

                Remote.Failure err ->
                    model => (Log.logMessage <| Log.LogMessage ("Model details response failure: " ++ toString err) Log.Error)

                _ ->
                    model => Cmd.none

        TogglePredict _ ->
            let
                predictModel =
                    case model.predictModel of
                        Nothing ->
                            Just (ModelPredict.init model.config model.modelId)

                        Just _ ->
                            Nothing
            in
            { model | predictModel = predictModel } => Cmd.none

        ModelPredictMsg subMsg ->
            let
                ( predictModel, cmd ) =
                    case model.predictModel of
                        Just predictModel ->
                            ModelPredict.update subMsg predictModel |> Tuple.mapFirst Just

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
                    Request.Model.delete model.config >> ignoreCascadeParams

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


view : Model -> Html Msg
view model =
    div []
        [ p [ class "breadcrumb" ]
            [ span []
                [ a [ href "#" ] [ text "Api Dashboard" ]
                ]
            , i [ class "fa fa-angle-right", style [ ( "margin", "0 5px" ) ] ] []
            , span [] [ a [ href "/#/models" ] [ text "Models" ] ]
            ]
        , div [ class "row" ]
            [ dataSourceName model
            , div [ class "col-sm-3" ]
                [ div [ class "mt10 right" ]
                    [ button [ class "btn", onClick (TogglePredict ()) ] [ text "Predict" ]
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-4" ]
                [ p [ class "small" ]
                    [ strong [] [ text "Model ID:" ]
                    , text (padSpace model.modelId)
                    , a [] [ i [ class "fa fa-copy color-mediumGray" ] [] ]
                    ]
                ]
            , div [ class "col-sm-4" ]
                [ p [ class "small" ]
                    [ strong [] [ text "Model Type:" ]
                    , text (padSpace (toString model.modelType))
                    ]
                ]
            , div [ class "col-sm-4 right" ]
                [ button [ class "btn btn-xs secondary", onClick ShowDeleteDialog ]
                    [ i [ class "fa fa-trash-o mr5" ] []
                    , text "Delete"
                    ]
                ]
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-4" ] (detailRow model)
            , div [ class "col-sm-5" ] []
            , Related.view model.config model.modelResponse
            ]
        , hr [] []
        , renderPredict model
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete Model"
            , bodyMessage = Just "This action cannot be undone. You will have to run another session to replace this model."
            , associatedAssets = []
            }
            |> Html.map DeleteDialogMsg
        ]


renderPredict : Model -> Html Msg
renderPredict model =
    case model.predictModel of
        Just predictModel ->
            ModelPredict.view predictModel |> Html.map ModelPredictMsg

        Nothing ->
            div [] []


dataSourceName : Model -> Html Msg
dataSourceName model =
    case model.modelResponse of
        Remote.Success resp ->
            div [ class "col-sm-9" ] [ h2 [ class "mt10" ] [ text ("Model for " ++ resp.dataSourceName) ] ]

        Remote.Loading ->
            text "Loading..."

        _ ->
            text "Not found"


detailRow : Model -> List (Html Msg)
detailRow model =
    case model.modelResponse of
        Remote.Success resp ->
            [ h5 [ class "mt15 mb15" ] [ text "Details" ]
            , p []
                [ strong [] [ text "Session Used:" ]
                , a [ Routes.href (Routes.SessionDetail resp.sessionId) ] [ text resp.sessionId ]
                ]
            , p []
                [ strong [] [ text "Source:" ]
                , a [ Routes.href (Routes.DataSetDetail (toDataSetName resp.dataSourceName)) ] [ text resp.dataSourceName ]
                ]
            , p []
                [ strong [] [ text "Target Column:" ]
                , text (find (\c -> c.role == Data.Columns.Target) resp.columns |> Maybe.map (\t -> t.name) |> Maybe.withDefault "")
                ]
            , p []
                [ strong [] [ text "Algorithm:" ]
                , text resp.algorithm.name
                ]
            , metricsList resp.metrics
            ]

        Remote.Loading ->
            [ text "Loading..." ]

        _ ->
            [ text "Not found" ]


metricsList : Dict String Float -> Html Msg
metricsList metrics =
    div []
        [ p [ class "small" ]
            [ strong [] [ text "Metrics" ]
            ]
        , ul [ class "small algorithm-metrics" ] (List.map metricListItem (Dict.toList metrics))
        ]


metricListItem : ( String, Float ) -> Html Msg
metricListItem ( name, value ) =
    li []
        [ strong [] [ text name ]
        , br [] []
        , text (formatFloatToString value)
        ]

padSpace : String -> String
padSpace input =
    " " ++ input ++ " "