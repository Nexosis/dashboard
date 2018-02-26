module Page.ModelDetail exposing (Model, Msg, init, update, view)

import AppRoutes as Routes
import Data.Columns exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.DataSet exposing (toDataSetName)
import Data.Model exposing (..)
import Data.PredictionDomain exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra exposing (find)
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (getOne)
import Util exposing ((=>))
import View.RelatedLinks as Related exposing (view)


type alias Model =
    { modelId : String
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelResponse response ->
            case response of
                Remote.Success modelInfo ->
                    { model | modelResponse = response, modelType = modelInfo.predictionDomain } => Cmd.none

                Remote.Failure err ->
                    model => (Log.logMessage <| Log.LogMessage ("Model details response failure: " ++ toString err) Log.Error)

                _ ->
                    model => Cmd.none


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
                    [ button [ class "btn", href "#/predict" ] [ text "Predict" ]
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
                [ button [ class "btn btn-xs secondary" ]
                    [ i [ class "fa fa-trash-o mr5" ] []
                    , text "Delete"
                    ]
                ]
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-4" ] (detailRow model)
            , div [ class "col-sm-5" ] []
            , Related.view model.modelResponse
            ]
        ]


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


formatFloatToString : Float -> String
formatFloatToString input =
    let
        expand =
            toString (ceiling (input * 100000))

        len =
            String.length expand

        filled =
            String.padLeft 5 '0' expand
    in
    String.left (len - 5) filled ++ "." ++ String.right 5 filled


padSpace : String -> String
padSpace input =
    " " ++ input ++ " "
