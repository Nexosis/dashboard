module Page.ModelPredict exposing (Model, Msg, init, subscriptions, update, view)

import Data.Config exposing (Config)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (succeed)
import Ports exposing (fileContentRead, uploadFileSelected)
import RemoteData as Remote
import Util exposing ((=>))


type alias Model =
    { modelId : String
    , activeTab : Tab
    , fileContent : String
    , fileName : String
    , fileUploadType : FileUploadType
    , fileUploadErrorOccurred : Bool
    , uploadResponse : Remote.WebData ()
    }


init : String -> Model
init modelId =
    Model modelId Upload "" "" Csv False Remote.NotAsked


type Msg
    = ChangeTab Tab
    | FileSelected
    | FileContentRead Json.Decode.Value
    | DataEntered
    | PredictionStarted


type Tab
    = Upload
    | PasteIn


type FileReadStatus
    = ReadError
    | Success String String


type FileUploadType
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


filenameToType : String -> FileUploadType
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


fileUploadTypeToContentType : FileUploadType -> String
fileUploadTypeToContentType uploadType =
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
            { model | activeTab = tab } => Cmd.none

        FileSelected ->
            model => Cmd.none

        FileContentRead content ->
            model => Cmd.none

        DataEntered ->
            model => Cmd.none

        PredictionStarted ->
            model => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileContentRead


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-sm-12" ] (predictWizard model) ]


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


viewPasteData : Model -> Html Msg
viewPasteData model =
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
                    [ text "How to enter data" ]
                , p []
                    [ text "Past instructions go here" ]
                ]
            ]
        ]
