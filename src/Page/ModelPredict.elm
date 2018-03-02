module Page.ModelPredict exposing (Model, Msg, init, subscriptions, update, view)

import Data.Config exposing (Config)
import Data.DataFormat as DataFormat
import Data.File as File
import Data.Model exposing (PredictionResult, decodePredictions)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode exposing (succeed)
import Ports exposing (fileContentRead, uploadFileSelected)
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (predict)
import Util exposing ((=>), spinner)
import View.Extra exposing (viewIf)
import View.Pager as Pager


type alias Model =
    { config : Config
    , modelId : String
    , activeTab : Tab
    , inputType : DataFormat.DataFormat
    , fileName : String
    , fileUploadErrorOccurred : Bool
    , dataInput : Maybe String
    , canProceedWithPrediction : Bool
    , uploadResponse : Remote.WebData Data.Model.PredictionResult
    , predictionData : List (Dict String String)
    }


init : Config -> String -> Model
init config modelId =
    Model config modelId UploadFile DataFormat.Json "" False Nothing False Remote.NotAsked [ Dict.empty ]


type Msg
    = ChangeTab Tab
    | FileSelected
    | FileContentRead Json.Decode.Value
    | DataInputChanged String
    | PredictionStarted
    | PredictResponse (Remote.WebData Data.Model.PredictionResult)
    | ResetState


type Tab
    = UploadFile
    | PasteIn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTab tab ->
            -- if we are switching to `Upload`, it's not available, if switching to `PasteIn`, it is available if content is available
            let
                canProceed =
                    case tab of
                        UploadFile ->
                            False

                        PasteIn ->
                            case model.dataInput of
                                Just _ ->
                                    True

                                Nothing ->
                                    False
            in
            { model | activeTab = tab, canProceedWithPrediction = canProceed } => Cmd.none

        FileSelected ->
            model => Ports.uploadFileSelected "upload-predict"

        FileContentRead readResult ->
            let
                readStatus =
                    Json.Decode.decodeValue File.fileReadStatusDecoder readResult
                        |> Result.withDefault File.ReadError

                m =
                    case readStatus of
                        File.Success fileName content ->
                            case File.filenameToType fileName of
                                DataFormat.Json ->
                                    { model
                                        | dataInput = Just content
                                        , inputType = DataFormat.Json
                                        , fileName = fileName
                                        , fileUploadErrorOccurred = False
                                        , canProceedWithPrediction = True
                                    }

                                DataFormat.Csv ->
                                    { model
                                        | dataInput = Just content
                                        , inputType = DataFormat.Csv
                                        , fileName = fileName
                                        , fileUploadErrorOccurred = False
                                        , canProceedWithPrediction = True
                                    }

                                DataFormat.Other ->
                                    { model | fileUploadErrorOccurred = True }

                        File.ReadError ->
                            { model | fileUploadErrorOccurred = True }
            in
            m => Cmd.none

        DataInputChanged content ->
            let
                trimmed =
                    content |> String.trim

                value =
                    if String.isEmpty trimmed then
                        Nothing
                    else
                        Just trimmed

                -- uses first non-whitespace character to see if it's JSON
                inputType =
                    if trimmed |> String.startsWith "{" then
                        DataFormat.Json
                    else
                        DataFormat.Csv
            in
            { model | dataInput = value, inputType = inputType, canProceedWithPrediction = not (String.isEmpty content) } => Cmd.none

        PredictionStarted ->
            let
                value =
                    case model.dataInput of
                        Just input ->
                            input

                        Nothing ->
                            ""

                predictRequest =
                    predict model.config model.modelId value (File.dataFormatToContentType model.inputType)
                        |> Remote.sendRequest
                        |> Cmd.map PredictResponse
            in
            { model | canProceedWithPrediction = False, uploadResponse = Remote.Loading } => predictRequest

        PredictResponse result ->
            case result of
                Remote.Success predictionData ->
                    { model | uploadResponse = result, predictionData = predictionData.data } => Cmd.none

                Remote.Failure err ->
                    { model | uploadResponse = result } => Log.logHttpError err

                _ ->
                    model => Cmd.none

        ResetState ->
            init model.config model.modelId => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileContentRead


view : Model -> Html Msg
view model =
    let
        waiting =
            if Remote.isLoading model.uploadResponse then
                spinner
            else
                div [] []
    in
    div [ class "row" ]
        [ div [ class "col-sm-12" ] (predictInput model)
        , div [ class "col-sm-12" ]
            [ button [ class "btn", disabled (not model.canProceedWithPrediction), onClick PredictionStarted ]
                [ text "Start predictions" ]
            , waiting
            , showTable model
            ]
        ]


predictInput : Model -> List (Html Msg)
predictInput model =
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


viewTabControl : Model -> Html Msg
viewTabControl model =
    let
        tabHeaders =
            [ li [ classList [ ( "active", model.activeTab == UploadFile ) ] ] [ a [ onClick (ChangeTab UploadFile) ] [ text "Upload" ] ]
            , li [ classList [ ( "active", model.activeTab == PasteIn ) ] ] [ a [ onClick (ChangeTab PasteIn) ] [ text "Paste Data" ] ]
            ]
    in
    ul [ class "nav nav-tabs", attribute "role" "tablist" ]
        tabHeaders


viewTabContent : Model -> Html Msg
viewTabContent model =
    let
        content =
            case model.activeTab of
                UploadFile ->
                    viewUploadTab model

                PasteIn ->
                    viewPasteData model
    in
    div [ class "tab-content" ]
        [ div [ class "tab-pane active" ]
            [ content ]
        ]


viewUploadTab : Model -> Html Msg
viewUploadTab model =
    let
        uploadButtonText =
            if String.isEmpty model.fileName then
                "Select your file"
            else
                model.fileName
    in
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-8" ]
                [ input
                    [ id "upload-predict"
                    , class "upload-file"
                    , type_ "file"
                    , on "change" (succeed FileSelected)
                    ]
                    []
                , label [ for "upload-predict" ] [ text uploadButtonText ]
                , viewIf (\() -> div [ class "alert alert-danger" ] [ text "An error occurred when uploading the file.  Please ensure it is a valid JSON or CSV file and try again." ]) model.fileUploadErrorOccurred
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


viewPasteData : Model -> Html Msg
viewPasteData model =
    let
        value =
            case model.dataInput of
                Just input ->
                    input

                Nothing ->
                    ""
    in
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group" ]
                [ label []
                    [ text "Enter Data" ]
                , textarea [ class "form-control", rows 20, cols 75, onInput DataInputChanged ]
                    [ text value ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ h5 []
                    [ text "How to enter data" ]
                , p []
                    [ text "Paste instructions go here" ]
                ]
            ]
        ]


showTable : Model -> Html Msg
showTable model =
    let
        output =
            case model.uploadResponse of
                Remote.Success successResponse ->
                    div [ class "row" ]
                        [ div [ class "col-sm-12" ]
                            [ div [ class "mt10" ]
                                [ {- button [ class "btn", disabled (not (Remote.isSuccess model.uploadResponse)) ]
                                         [ i [ class "fa fa-download mr5" ]
                                             []
                                         , text "Download predictions"
                                         ]
                                     ,
                                  -}
                                  button [ class "btn secondary", onClick ResetState ]
                                    [ i [ class "fa fa-refresh mr5" ]
                                        []
                                    , text "Predict again"
                                    ]
                                ]
                            ]
                        , div [ class "col-sm-12" ]
                            [ h3 []
                                [ text "Results" ]
                            ]
                        , div [ class "col-sm-12" ]
                            [ table [ class "table table-striped" ]
                                [ toTableHeader (List.head successResponse.data)
                                , tbody [] (List.map toTableRow successResponse.data)
                                ]
                            ]
                        ]

                _ ->
                    div [ class "row" ] []
    in
    output


toTableHeader : Maybe (Dict String String) -> Html Msg
toTableHeader item =
    let
        keys =
            case item of
                Just row ->
                    Dict.keys row

                Nothing ->
                    []
    in
    thead [] (List.map toTableHeaderItem keys)


toTableRow : Dict String String -> Html Msg
toTableRow item =
    tr [] (List.map toTableData (Dict.values item))


toTableHeaderItem : String -> Html Msg
toTableHeaderItem value =
    th [] [ text value ]


toTableData : String -> Html Msg
toTableData value =
    td [] [ text value ]
