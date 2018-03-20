module Page.ModelPredict exposing (Model, Msg, init, subscriptions, update, view)

import Data.Config exposing (Config)
import Data.Context exposing (ContextModel)
import Data.DataFormat as DataFormat
import Data.File as File
import Data.Model exposing (PredictionResult, decodePredictions)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode exposing (succeed)
import Page.Helpers exposing (expandedMessagesTable, explainer)
import Ports exposing (fileContentRead, fileSaved, prismHighlight, requestSaveFile, scrollIntoView, uploadFileSelected)
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (predict, predictRaw)
import Util exposing ((=>), isJust, spinner)
import View.Error exposing (viewRemoteError)
import View.Extra exposing (viewIf)
import View.Pager as Pager exposing (PagedListing, filterToPage, mapToPagedListing)


type alias Model =
    { modelId : String
    , activeTab : Tab
    , inputType : DataFormat.DataFormat
    , outputType : DataFormat.DataFormat
    , fileName : String
    , fileUploadErrorOccurred : Bool
    , dataInput : Maybe String
    , uploadResponse : Remote.WebData Data.Model.PredictionResult
    , downloadResponse : Remote.WebData String
    , showDownloadTypeSelector : Bool
    , currentPage : Int
    }


type alias PredictionResultListing =
    { pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    , predictions : List (Dict String String)
    }


init : Config -> String -> Model
init config modelId =
    Model modelId UploadFile DataFormat.Json DataFormat.Json "" False Nothing Remote.NotAsked Remote.NotAsked False 0


type Msg
    = ChangeTab Tab
    | FileSelected
    | FileContentRead Json.Decode.Value
    | DataInputChanged String
    | PredictionStarted
    | PredictResponse (Remote.WebData Data.Model.PredictionResult)
    | ChangePage Int
    | ToggleFileTypeSelector
    | FileDownload DataFormat.DataFormat
    | DownloadComplete Bool
    | DownloadResponse (Remote.WebData String)


type Tab
    = UploadFile
    | PasteIn


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        ChangeTab tab ->
            { model | activeTab = tab } => prismHighlight ()

        FileSelected ->
            model => Ports.uploadFileSelected "upload-dataset"

        FileContentRead readResult ->
            let
                readStatus =
                    Json.Decode.decodeValue File.fileReadStatusDecoder readResult
                        |> Result.withDefault File.ReadError

                m =
                    case readStatus of
                        File.Success fileName content ->
                            case DataFormat.filenameToType fileName of
                                DataFormat.Json ->
                                    { model
                                        | dataInput = Just content
                                        , inputType = DataFormat.Json
                                        , fileName = fileName
                                        , fileUploadErrorOccurred = False
                                    }

                                DataFormat.Csv ->
                                    { model
                                        | dataInput = Just content
                                        , inputType = DataFormat.Csv
                                        , fileName = fileName
                                        , fileUploadErrorOccurred = False
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
            { model | dataInput = value, inputType = inputType } => Cmd.none

        PredictionStarted ->
            let
                value =
                    Maybe.withDefault "" model.dataInput

                predictRequest =
                    predict context.config model.modelId value (DataFormat.dataFormatToContentType model.inputType)
                        |> Remote.sendRequest
                        |> Cmd.map PredictResponse
            in
            { model | uploadResponse = Remote.Loading } => predictRequest

        ToggleFileTypeSelector ->
            { model | showDownloadTypeSelector = not model.showDownloadTypeSelector } => Cmd.none

        FileDownload downloadType ->
            let
                value =
                    Maybe.withDefault "" model.dataInput

                predictRequest =
                    predictRaw context.config model.modelId value (DataFormat.dataFormatToContentType model.inputType) (DataFormat.dataFormatToContentType downloadType)
                        |> Remote.sendRequest
                        |> Cmd.map DownloadResponse
            in
            { model | downloadResponse = Remote.Loading, outputType = downloadType, showDownloadTypeSelector = False } => predictRequest

        PredictResponse result ->
            case result of
                Remote.Success predictionData ->
                    { model | uploadResponse = result }
                        => scrollIntoView "results"

                Remote.Failure err ->
                    { model | uploadResponse = result } => Log.logHttpError err

                _ ->
                    model => Cmd.none

        DownloadResponse result ->
            case result of
                Remote.Success predictionData ->
                    { model | downloadResponse = result } => Ports.requestSaveFile { contents = predictionData, name = formatFilename model, contentType = DataFormat.dataFormatToContentType model.outputType }

                Remote.Failure err ->
                    { model | downloadResponse = result } => Log.logHttpError err

                _ ->
                    model => Cmd.none

        DownloadComplete success ->
            model => Cmd.none

        ChangePage pageNum ->
            { model | currentPage = pageNum } => Cmd.none


formatFilename : Model -> String
formatFilename model =
    model.modelId ++ "-results." ++ DataFormat.dataFormatToString model.outputType


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fileContentRead FileContentRead
        , fileSaved DownloadComplete
        ]


view : Model -> ContextModel -> Html Msg
view model context =
    let
        buttonText =
            case model.uploadResponse of
                Remote.NotAsked ->
                    text "Start predictions"

                Remote.Loading ->
                    spinner

                Remote.Failure _ ->
                    text "Start predictions"

                Remote.Success _ ->
                    text "Predict again"

        canProceedWithPrediction =
            not (Remote.isLoading model.downloadResponse || Remote.isLoading model.uploadResponse)
                && (model.dataInput |> Maybe.map (\d -> not <| String.isEmpty d) |> Maybe.withDefault False)

        predictButton =
            div [ class "mt5 right" ] [ button [ class "btn btn-danger", disabled <| not canProceedWithPrediction, onClick PredictionStarted ] [ buttonText ] ]
    in
    div [ class "row" ]
        [ div [ class "col-sm-12" ] (viewPredictInput context model predictButton)
        , div [ class "col-sm-12" ] [ viewRemoteError model.uploadResponse ]
        , div [ class "col-sm-9" ] []
        , div [ class "col-sm-3" ] [ predictButton ]
        , div [ id "results", class "col-sm-12" ] [ showTable model ]
        ]


viewPredictInput : ContextModel -> Model -> Html Msg -> List (Html Msg)
viewPredictInput context model predictButton =
    [ div [ id "predict" ]
        [ div [ class "row" ]
            [ div [ class "col-sm-9" ]
                [ h3 [] [ text "Run a prediction" ] ]
            , div [ class "col-sm-3" ] [ predictButton ]
            , div [ class "col-sm-12" ]
                [ h4 []
                    [ text "Choose your prediction file" ]
                ]
            , div [ class "col-sm-12" ]
                [ viewTabControl model
                , viewTabContent context model
                ]
            ]
        ]
    ]


viewTabControl : Model -> Html Msg
viewTabControl model =
    let
        tabHeaders =
            [ li [ classList [ ( "active", model.activeTab == UploadFile ) ] ] [ a [ attribute "role" "button", onClick (ChangeTab UploadFile) ] [ text "Upload" ] ]
            , li [ classList [ ( "active", model.activeTab == PasteIn ) ] ] [ a [ attribute "role" "button", onClick (ChangeTab PasteIn) ] [ text "Paste Data" ] ]
            ]
    in
    ul [ class "nav nav-tabs", attribute "role" "tablist" ]
        tabHeaders


viewTabContent : ContextModel -> Model -> Html Msg
viewTabContent context model =
    let
        content =
            case model.activeTab of
                UploadFile ->
                    viewUploadTab context model

                PasteIn ->
                    viewPasteData context model
    in
    div [ class "tab-content" ]
        [ div [ class "tab-pane active" ]
            [ content ]
        ]


viewUploadTab : ContextModel -> Model -> Html Msg
viewUploadTab context model =
    let
        uploadButtonText =
            if String.isEmpty model.fileName then
                "Select your file"
            else
                model.fileName
    in
    div [ class "row" ]
        [ div [ class "col-sm-6 pl0" ]
            [ div [ class "form-group" ]
                [ input
                    [ id "upload-dataset"
                    , class "upload-dataset"
                    , type_ "file"
                    , on "change" (succeed FileSelected)
                    ]
                    []
                , label [ for "upload-dataset" ] [ text uploadButtonText ]
                , viewIf (\() -> div [ class "alert alert-danger" ] [ text "An error occurred when uploading the file.  Please ensure it is a valid JSON or CSV file and try again." ]) model.fileUploadErrorOccurred
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "how_upload_csv" ]
            ]
        ]


viewPasteData : ContextModel -> Model -> Html Msg
viewPasteData context model =
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
                [ explainer context.config "how_paste_data"
                ]
            ]
        ]


showTable : Model -> Html Msg
showTable model =
    let
        pagedData =
            Remote.map (.data >> mapToPagedListing model.currentPage) model.uploadResponse
    in
    case model.uploadResponse of
        Remote.Success successResponse ->
            div []
                [ div [ class "row" ]
                    [ hr [] []
                    , div [ class "col-sm-6" ]
                        [ expandedMessagesTable "predictMessages" successResponse.messages ]
                    , div [ class "col-sm-6 center" ]
                        [ div [ class "alert alert-danger" ]
                            [ h5 [ class "mt15 mb15 center" ]
                                [ text "Prediction results are not stored and must be downloaded for future usage." ]
                            , div [ class "mt10" ]
                                [ downloadButton model ]
                            ]
                        ]
                    , hr [] []
                    ]
                , div [ class "row" ]
                    [ div [ class "col-sm-12" ]
                        [ h3 []
                            [ text "Results" ]
                        ]
                    ]
                , div [ class "row" ]
                    [ div [ class "col-sm-12" ]
                        [ table [ class "table table-striped" ]
                            [ toTableHeader (List.head successResponse.data)
                            , tbody [] (List.map toTableRow (filterToPage pagedData))
                            ]
                        , div [ class "center" ] [ Pager.view pagedData ChangePage ]
                        ]
                    ]
                ]

        _ ->
            div [ class "row" ] []


downloadButton : Model -> Html Msg
downloadButton model =
    let
        dropDownClass =
            if model.showDownloadTypeSelector then
                "btn-group open"
            else
                "btn-group"
    in
    if Remote.isLoading model.downloadResponse then
        spinner
    else
        div [ class dropDownClass ]
            [ button [ class "btn btn-danger", onClick <| FileDownload DataFormat.Json ]
                [ i [ class "fa fa-download mr5" ]
                    []
                , text "Download predictions"
                ]
            , button [ attribute "aria-expanded" "false", attribute "aria-haspopup" "true", class "btn btn-danger dropdown-toggle", attribute "data-toggle" "dropdown", type_ "button", onClick ToggleFileTypeSelector ]
                [ span [ class "caret" ]
                    []
                , span [ class "sr-only" ]
                    [ text "Toggle Dropdown" ]
                ]
            , ul [ class "dropdown-menu" ]
                [ li []
                    [ a [ onClick <| FileDownload DataFormat.Csv ]
                        [ text "CSV" ]
                    ]
                , li []
                    [ a [ onClick <| FileDownload DataFormat.Json ]
                        [ text "JSON" ]
                    ]
                ]
            ]


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
