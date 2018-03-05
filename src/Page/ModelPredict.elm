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
import Ports exposing (fileContentRead, fileSaved, requestSaveFile, uploadFileSelected)
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (predict, predictRaw)
import Util exposing ((=>), spinner)
import View.Extra exposing (viewIf)
import View.Messages as Messages
import View.Pager as Pager


type alias Model =
    { config : Config
    , modelId : String
    , activeTab : Tab
    , processState : State
    , inputType : DataFormat.DataFormat
    , outputType : DataFormat.DataFormat
    , fileName : String
    , fileUploadErrorOccurred : Bool
    , dataInput : Maybe String
    , canProceedWithPrediction : Bool
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
    Model config modelId UploadFile Initialized DataFormat.Json DataFormat.Json "" False Nothing False Remote.NotAsked Remote.NotAsked False 0


type Msg
    = ChangeTab Tab
    | FileSelected
    | FileContentRead Json.Decode.Value
    | DataInputChanged String
    | PredictionStarted
    | PredictResponse (Remote.WebData Data.Model.PredictionResult)
    | ChangePage Int
    | SetDownloadContentType DataFormat.DataFormat
    | ToggleFileTypeSelector
    | FileDownload
    | DownloadComplete Bool
    | DownloadResponse (Remote.WebData String)
    | PredictAgain


type Tab
    = UploadFile
    | PasteIn


type State
    = Initialized
    | PredictionsStarted
    | PredictionsComplete
    | FileDownloaded


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
                            case DataFormat.filenameToType fileName of
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
                    predict model.config model.modelId value (DataFormat.dataFormatToContentType model.inputType)
                        |> Remote.sendRequest
                        |> Cmd.map PredictResponse
            in
            { model | canProceedWithPrediction = False, uploadResponse = Remote.Loading, processState = PredictionsStarted } => predictRequest

        ToggleFileTypeSelector ->
            { model | showDownloadTypeSelector = not model.showDownloadTypeSelector } => Cmd.none

        SetDownloadContentType value ->
            { model | outputType = value, showDownloadTypeSelector = False } => Cmd.none

        FileDownload ->
            let
                value =
                    case model.dataInput of
                        Just input ->
                            input

                        Nothing ->
                            ""

                predictRequest =
                    predictRaw model.config model.modelId value (DataFormat.dataFormatToContentType model.outputType)
                        |> Remote.sendRequest
                        |> Cmd.map DownloadResponse
            in
            { model | canProceedWithPrediction = False, downloadResponse = Remote.Loading } => predictRequest

        PredictResponse result ->
            case result of
                Remote.Success predictionData ->
                    { model | processState = PredictionsComplete, uploadResponse = result } => Cmd.none

                Remote.Failure err ->
                    { model | processState = PredictionsComplete, uploadResponse = result } => Log.logHttpError err

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
            { model | processState = FileDownloaded } => Cmd.none

        ChangePage pageNum ->
            { model | currentPage = pageNum } => Cmd.none

        PredictAgain ->
            { model | processState = Initialized, uploadResponse = Remote.NotAsked, downloadResponse = Remote.NotAsked, outputType = DataFormat.Json } => Cmd.none


formatFilename : Model -> String
formatFilename model =
    model.modelId ++ "-results." ++ DataFormat.dataFormatToString model.outputType


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.processState of
        Initialized ->
            fileContentRead FileContentRead

        PredictionsComplete ->
            fileSaved DownloadComplete

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    let
        ( buttonText, action ) =
            case model.processState of
                Initialized ->
                    text "Start predictions" => PredictionStarted

                PredictionsStarted ->
                    spinner => PredictionStarted

                PredictionsComplete ->
                    text "Predict again" => PredictAgain

                FileDownloaded ->
                    text "Predict again" => PredictAgain
    in
    div [ class "row" ]
        [ div [ class "col-sm-12" ] (predictInput model)
        , div [ class "col-sm-12" ]
            [ button [ class "btn", disabled (model.processState == PredictionsStarted), onClick action ]
                [ buttonText ]
            , showTable model
            ]
        ]


predictInput : Model -> List (Html Msg)
predictInput model =
    case model.processState of
        Initialized ->
            viewPredictInput model

        PredictionsStarted ->
            viewPredictInput model

        _ ->
            [ div [] [] ]


viewPredictInput : Model -> List (Html Msg)
viewPredictInput model =
    [ div [ id "predict" ]
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


mapToPagedListing : Int -> List (Dict String String) -> PredictionResultListing
mapToPagedListing currentPage rows =
    let
        count =
            List.length rows

        pageSize =
            10
    in
    { pageNumber = currentPage
    , totalPages = count // pageSize
    , pageSize = pageSize
    , totalCount = count
    , predictions = rows
    }


filterToPage : Remote.WebData PredictionResultListing -> List (Dict String String)
filterToPage model =
    case model of
        Remote.Success result ->
            let
                drop =
                    result.pageSize * result.pageNumber
            in
            result.predictions
                |> List.drop drop
                |> List.take result.pageSize

        _ ->
            [ Dict.empty ]


showTable : Model -> Html Msg
showTable model =
    let
        pagedData =
            Remote.map (.data >> mapToPagedListing model.currentPage) model.uploadResponse
    in
    case model.uploadResponse of
        Remote.Success successResponse ->
            div [ id "results" ]
                [ div [ class "row" ]
                    [ hr [] []
                    , div [ class "col-sm-6" ]
                        [ h5 [ class "mt15 mb15" ]
                            [ Messages.viewMessages successResponse.messages ]
                        ]
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
            case model.showDownloadTypeSelector of
                True ->
                    "btn-group open"

                _ ->
                    "btn-group"
    in
    div [ class dropDownClass ]
        [ button [ class "btn", onClick FileDownload ]
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
                [ a [ onClick (SetDownloadContentType DataFormat.Csv) ]
                    [ text "CSV" ]
                ]
            , li []
                [ a [ onClick (SetDownloadContentType DataFormat.Json) ]
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
