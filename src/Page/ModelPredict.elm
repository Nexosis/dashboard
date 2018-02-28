module Page.ModelPredict exposing (Model, Msg, init, subscriptions, update, view)

import Data.Config exposing (Config)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode exposing (succeed)
import Ports exposing (fileContentRead, uploadFileSelected)
import RemoteData as Remote
import Request.Model exposing (predict)
import Util exposing ((=>))
import View.Extra exposing (viewIf)


type alias Model =
    { config : Config
    , modelId : String
    , activeTab : Tab
    , inputType : InputType
    , fileName : String
    , fileUploadErrorOccurred : Bool
    , dataInput : Maybe String
    , canProceedWithPrediction : Bool
    , uploadResponse : Remote.WebData ()
    }


init : Config -> String -> Model
init config modelId =
    Model config modelId Upload Json "" False Nothing False Remote.NotAsked


type Msg
    = ChangeTab Tab
    | FileSelected
    | FileContentRead Json.Decode.Value
    | InputTypeSelected InputType
    | DataInputChanged String
    | DataEntered
    | PredictionStarted
    | PredictResponse (Remote.WebData ())


type Tab
    = Upload
    | PasteIn


type FileReadStatus
    = ReadError
    | Success String String


type InputType
    = Json
    | Csv
    | Other


fileReadStatusDecoder : Json.Decode.Decoder FileReadStatus
fileReadStatusDecoder =
    Json.Decode.field "status" Json.Decode.string
        |> Json.Decode.andThen
            (\status ->
                case status of
                    "Success" ->
                        Json.Decode.map2
                            (\f c -> Success f c)
                            (Json.Decode.field "filename" Json.Decode.string)
                            (Json.Decode.field "contents" Json.Decode.string)

                    _ ->
                        Json.Decode.succeed ReadError
            )


filenameToType : String -> InputType
filenameToType name =
    let
        lowerString =
            String.toLower name
    in
    if String.endsWith ".json" lowerString then
        Json
    else if String.endsWith ".csv" lowerString then
        Csv
    else
        Other


inputTypeToContentType : InputType -> String
inputTypeToContentType uploadType =
    case uploadType of
        Json ->
            "application/json"

        Csv ->
            "text/csv"

        _ ->
            ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTab tab ->
            -- if we are switching to `Upload`, it's not available, if switching to `PasteIn`, it is available if content is available
            let
                canProceed =
                    case tab of
                        Upload ->
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
                    Json.Decode.decodeValue fileReadStatusDecoder readResult
                        |> Result.withDefault ReadError

                m =
                    case readStatus of
                        Success fileName content ->
                            case filenameToType fileName of
                                Json ->
                                    { model
                                        | dataInput = Just content
                                        , inputType = Json
                                        , fileName = fileName
                                        , fileUploadErrorOccurred = False
                                        , canProceedWithPrediction = True
                                    }

                                Csv ->
                                    { model
                                        | dataInput = Just content
                                        , inputType = Csv
                                        , fileName = fileName
                                        , fileUploadErrorOccurred = False
                                        , canProceedWithPrediction = True
                                    }

                                Other ->
                                    { model | fileUploadErrorOccurred = True }

                        ReadError ->
                            { model | fileUploadErrorOccurred = True }
            in
            m => Cmd.none

        InputTypeSelected selection ->
            { model | inputType = selection } => Cmd.none

        DataInputChanged content ->
            let
                value =
                    if String.isEmpty content then
                        Nothing
                    else
                        Just content
            in
            { model | dataInput = value, canProceedWithPrediction = not (String.isEmpty content) } => Cmd.none

        DataEntered ->
            model => Cmd.none

        PredictionStarted ->
            let
                value =
                    case model.dataInput of
                        Just input ->
                            input

                        Nothing ->
                            ""

                predictRequest =
                    predict model.config model.modelId value (inputTypeToContentType model.inputType)
                        |> Remote.sendRequest
                        |> Cmd.map PredictResponse
            in
            { model | uploadResponse = Remote.Loading } => predictRequest

        PredictResponse response ->
            model => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileContentRead


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-sm-12" ] (predictWizard model)
        , div [ class "col-sm-12" ]
            [ button [ class "btn", disabled (not model.canProceedWithPrediction), onClick PredictionStarted ]
                [ text "Start predictions" ]
            ]
        ]


predictWizard : Model -> List (Html Msg)
predictWizard model =
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
            [ li [ classList [ ( "active", model.activeTab == Upload ) ] ] [ a [ onClick (ChangeTab Upload) ] [ text "Upload" ] ]
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
                Upload ->
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
