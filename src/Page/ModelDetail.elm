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
import Html.Events exposing (onClick)
import Json.Decode exposing (succeed)
import List.Extra exposing (find)
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (getOne)
import Util exposing ((=>))


type alias Model =
    { modelId : String
    , modelResponse : Remote.WebData ModelData
    , config : Config
    , modelType : PredictionDomain
    , predictShown : Bool
    , activeTab : Tab
    , fileContent : String
    , fileName : String
    , fileUploadType : FileUploadType
    , fileUploadErrorOccurred : Bool
    , uploadResponse : Remote.WebData ()
    }


type Tab
    = Upload
    | Import



--    | PasteIn


type FileReadStatus
    = ReadError
    | Success String String


type FileUploadType
    = Json
    | Csv
    | Other


init : Config -> String -> ( Model, Cmd Msg )
init config modelId =
    let
        loadModelDetail =
            Request.Model.getOne config modelId
                |> Remote.sendRequest
                |> Cmd.map ModelResponse
    in
    Model modelId Remote.Loading config Regression False Upload "" "" Csv False Remote.NotAsked => loadModelDetail


type Msg
    = ModelResponse (Remote.WebData ModelData)
    | TogglePredict
    | ChangeTab Tab
    | FileSelected
    | FileContentRead Json.Decode.Value
    | PredictionStarted
    | PredictionComplete (Remote.WebData ())


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

        TogglePredict ->
            { model | predictShown = not model.predictShown } => Cmd.none

        ChangeTab tab ->
            { model | activeTab = tab } => Cmd.none

        FileSelected ->
            model => Cmd.none

        FileContentRead content ->
            model => Cmd.none

        PredictionStarted ->
            model => Cmd.none

        PredictionComplete _ ->
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
                    [ button [ class "btn", onClick TogglePredict ] [ text "Predict" ]
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
            , div [ class "col-sm-3" ] []
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12" ] (predictWizard model)
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


viewTabControl : Model -> Html Msg
viewTabControl model =
    let
        tabHeaders =
            [ li [ classList [ ( "active", model.activeTab == Upload ) ] ] [ a [ onClick (ChangeTab Upload) ] [ text "Upload" ] ]
            , li [ classList [ ( "active", model.activeTab == Import ) ] ] [ a [ onClick (ChangeTab Import) ] [ text "Import from URL" ] ]

            --, li [ classList [ ( "active", model.activeTab == PasteIn) ] ] [ a [ onClick (ChangeTab PasteIn) ] [ text "Paste Data" ] ]
            ]
    in
    ul [ class "nav nav-tabs", attribute "role" "tablist" ]
        tabHeaders


viewTabContent : Model -> Html Msg
viewTabContent model =
    let
        content =
            case model.activeTab of
                Upload ->
                    viewUploadTab model

                Import ->
                    viewImportUrlTab model

        -- PasteIn ->
        --     viewPasteInTab model
    in
    div [ class "tab-content" ]
        [ div [ class "tab-pane active" ]
            [ content ]
        ]


viewUploadTab : Model -> Html Msg
viewUploadTab model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-8" ]
                [ input [ class "upload-dataset", id "upload-dataset", type_ "file" ]
                    []
                , label [ for "upload-dataset" ]
                    [ text "Select your file" ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ h5 []
                    [ text "How to upload a CSV" ]
                , p []
                    [ text "CSV instructions go here" ]
                ]
            ]
        ]


viewImportUrlTab : Model -> Html Msg
viewImportUrlTab model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-8" ]
                [ label []
                    [ text "File URL" ]
                , input [ class "form-control", type_ "text" ]
                    []
                ]
            , div [ class "form-group col-sm-8" ]
                [ button [ class "btn" ]
                    [ i [ class "fa fa-upload mr5" ]
                        []
                    , text "Import"
                    ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ h5 []
                    [ text "How to import from a URL" ]
                , p []
                    [ text "URL instructions go here" ]
                ]
            ]
        ]


predictWizard : Model -> List (Html Msg)
predictWizard model =
    if model.predictShown then
        [ div [ class "collapse in", id "predict" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-12" ]
                    [ h3 [ class "mt0" ]
                        [ text "Choose prediction file" ]
                    ]
                , div [ class "col-sm-12" ]
                    [ viewTabControl model
                    , viewTabContent model
                    ]
                ]
            ]
        ]
    else
        [ div [] [] ]


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
