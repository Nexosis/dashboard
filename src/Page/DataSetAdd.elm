module Page.DataSetAdd exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes exposing (Route)
import Data.Config exposing (Config)
import Data.DataFormat as DataFormat
import Data.DataSet
import Data.File as File
import Data.Import
import Data.Status as Status
import Data.Ziplist as Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Http
import Json.Decode exposing (succeed)
import Navigation
import Ports exposing (fileContentRead, uploadFileSelected)
import Process
import Regex
import RemoteData as Remote
import Request.DataSet exposing (put)
import Request.Import
import Request.Log as Log
import Task exposing (Task)
import Time
import Util exposing ((=>), spinner)
import Validate exposing (Validator, ifBlank, validate)
import View.Error exposing (viewMessagesAsError, viewRemoteError)
import View.Extra exposing (viewIf)
import View.Wizard exposing (WizardConfig, viewButtons)


type alias Model =
    { config : Config
    , steps : Ziplist Step
    , canAdvance : Bool
    , name : String
    , key : String
    , activeTab : Tab
    , activeTabName : String
    , uploadResponse : Remote.WebData ()
    , importResponse : Remote.WebData Data.Import.ImportDetail
    }


type alias FileUploadEntry =
    { fileContent : String
    , fileName : String
    , fileUploadType : DataFormat.DataFormat
    , fileUploadErrorOccurred : Bool
    }


type alias UrlImportEntry =
    { importUrl : String }


type Step
    = ChooseUploadType
    | SetKey


type Tab
    = FileUploadTab FileUploadEntry
    | UrlImportTab UrlImportEntry


initFileUploadTab : FileUploadEntry
initFileUploadTab =
    { fileContent = ""
    , fileName = ""
    , fileUploadType = DataFormat.Other
    , fileUploadErrorOccurred = False
    }


initImportTab : UrlImportEntry
initImportTab =
    { importUrl = "" }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        steps =
            Ziplist.create [] ChooseUploadType [ SetKey ]
    in
    Model config
        steps
        False
        ""
        ""
        (FileUploadTab initFileUploadTab)
        "Upload"
        Remote.NotAsked
        Remote.NotAsked
        => Cmd.none



-- Validation


rootModelValid : Validator String Model
rootModelValid =
    Validate.all
        [ ifBlank .name "DataSet name required." ]


fileUploadModelValid : Validator String FileUploadEntry
fileUploadModelValid =
    Validate.all
        [ ifBlank .fileContent "Choose a file to upload." ]


urlImportModelValid : Validator String UrlImportEntry
urlImportModelValid =
    Validate.all
        [ Validate.ifFalse (\model -> model.importUrl |> Regex.contains urlRegex) "Enter a valid url." ]


urlRegex : Regex.Regex
urlRegex =
    -- Just do a simple check for something that starts with http or https, and doesn't have spaces.
    Regex.regex "^(http|https):\\/\\/[^ \"]+$"


setModelValid : Model -> Model
setModelValid model =
    let
        tabValidation =
            case model.activeTab of
                FileUploadTab fileUploadEntry ->
                    validate fileUploadModelValid fileUploadEntry

                UrlImportTab urlImportEntry ->
                    validate urlImportModelValid urlImportEntry
    in
    { model | canAdvance = tabValidation ++ validate rootModelValid model |> List.isEmpty }



-- Update


type Msg
    = ChangeName String
    | ChangeKey String
    | NextStep
    | PrevStep
    | ChangeTab ( Tab, String )
    | FileSelected
    | TabMsg TabMsg
    | CreateDataSet
    | UploadDataSetResponse (Remote.WebData ())
    | ImportResponse (Remote.WebData Data.Import.ImportDetail)


type TabMsg
    = FileContentRead Json.Decode.Value
    | ImportUrlInputChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.steps.current, msg ) of
        ( ChooseUploadType, ChangeName name ) ->
            { model | name = name }
                |> setModelValid
                => Cmd.none

        ( ChooseUploadType, ChangeTab ( tab, tabName ) ) ->
            { model | activeTab = tab, activeTabName = tabName, canAdvance = False } => Cmd.none

        ( ChooseUploadType, FileSelected ) ->
            model => Ports.uploadFileSelected "upload-dataset"

        ( ChooseUploadType, TabMsg tabMsg ) ->
            updateTabContents model tabMsg => Cmd.none

        ( SetKey, ChangeKey key ) ->
            { model | key = key } => Cmd.none

        ( SetKey, CreateDataSet ) ->
            case model.activeTab of
                FileUploadTab uploadInfo ->
                    let
                        putRequest =
                            put model.config model.name uploadInfo.fileContent (File.dataFormatToContentType uploadInfo.fileUploadType)
                                |> Remote.sendRequest
                                |> Cmd.map UploadDataSetResponse
                    in
                    { model | uploadResponse = Remote.Loading } => putRequest

                UrlImportTab urlTab ->
                    let
                        importRequest =
                            Request.Import.postUrl model.config model.name urlTab.importUrl
                                |> Remote.sendRequest
                                |> Cmd.map ImportResponse
                    in
                    { model | importResponse = Remote.Loading } => importRequest

        ( _, UploadDataSetResponse result ) ->
            case result of
                Remote.Success () ->
                    let
                        loadCmd =
                            Navigation.load <| AppRoutes.routeToString (AppRoutes.DataSetDetail <| Data.DataSet.toDataSetName model.name)
                    in
                    model => loadCmd

                Remote.Failure err ->
                    { model | uploadResponse = result } => Log.logHttpError err

                _ ->
                    model => Cmd.none

        ( _, ImportResponse result ) ->
            let
                cmd =
                    case result of
                        Remote.Success importDetail ->
                            if importDetail.status == Status.Completed then
                                AppRoutes.newUrl (AppRoutes.DataSetDetail importDetail.dataSetName)
                            else if importDetail.status == Status.Cancelled || importDetail.status == Status.Failed then
                                Cmd.none
                            else
                                delayAndRecheckImport model.config importDetail.importId

                        _ ->
                            Cmd.none
            in
            { model | importResponse = result } => cmd

        ( _, NextStep ) ->
            { model | steps = Ziplist.advance model.steps } => Cmd.none

        ( _, PrevStep ) ->
            { model | steps = Ziplist.rewind model.steps } => Cmd.none

        _ ->
            model => Cmd.none


delayAndRecheckImport : Config -> String -> Cmd Msg
delayAndRecheckImport config importId =
    delayTask
        |> Task.andThen (\_ -> Request.Import.get config importId |> Http.toTask)
        |> Remote.asCmd
        |> Cmd.map ImportResponse


delayTask : Task.Task x ()
delayTask =
    Process.sleep (Time.second * 2)


updateTabContents : Model -> TabMsg -> Model
updateTabContents model msg =
    let
        tabModel =
            case ( model.activeTab, msg ) of
                ( FileUploadTab fileUploadEntry, FileContentRead readResult ) ->
                    let
                        readStatus =
                            Json.Decode.decodeValue File.fileReadStatusDecoder readResult
                                |> Result.withDefault File.ReadError

                        m =
                            case readStatus of
                                File.Success fileName content ->
                                    case File.filenameToType fileName of
                                        DataFormat.Json ->
                                            { fileContent = content
                                            , fileName = fileName
                                            , fileUploadType = DataFormat.Json
                                            , fileUploadErrorOccurred = False
                                            }

                                        DataFormat.Csv ->
                                            { fileContent = content
                                            , fileName = fileName
                                            , fileUploadType = DataFormat.Csv
                                            , fileUploadErrorOccurred = False
                                            }

                                        DataFormat.Other ->
                                            { fileUploadEntry | fileUploadErrorOccurred = True }

                                File.ReadError ->
                                    { fileUploadEntry | fileUploadErrorOccurred = True }

                        valid =
                            validate fileUploadModelValid m |> List.isEmpty
                    in
                    FileUploadTab m

                ( UrlImportTab urlTab, ImportUrlInputChange urlEntry ) ->
                    let
                        urlImportModel =
                            { urlTab | importUrl = urlEntry }

                        valid =
                            validate urlImportModelValid urlImportModel |> List.isEmpty
                    in
                    UrlImportTab urlImportModel

                ( _, _ ) ->
                    model.activeTab
    in
    setModelValid { model | activeTab = tabModel }


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead <| \c -> TabMsg (FileContentRead c)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-sm-6" ] [ h2 [ class "mt10" ] [ text "Add DataSet" ] ]
            ]
        , hr [] []
        , div [ class "row" ]
            [ case model.steps.current of
                ChooseUploadType ->
                    viewChooseUploadType model

                SetKey ->
                    viewSetKey model
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ viewButtons configWizard model.canAdvance model.steps ]
            ]
        ]


viewChooseUploadType : Model -> Html Msg
viewChooseUploadType model =
    div []
        [ div [ class "col-sm-12" ]
            [ h3 [ class "mt0" ] [ text "Choose Upload type" ]
            , div [ class "form-group col-sm-4" ]
                [ label [] [ text "DataSet Name" ]
                , input [ class "form-control", onInput ChangeName, value model.name ] []
                ]
            ]
        , div [ class "col-sm-12" ]
            [ viewTabControl model
            , viewTabContent model
            ]
        ]


viewSetKey : Model -> Html Msg
viewSetKey model =
    let
        ( buttonAttributes, createButtonContent, errorDisplay ) =
            case model.activeTab of
                FileUploadTab _ ->
                    case model.uploadResponse of
                        Remote.Loading ->
                            ( [], spinner, viewRemoteError model.uploadResponse )

                        _ ->
                            ( [ onClick CreateDataSet ], text "Create DataSet", viewRemoteError model.uploadResponse )

                _ ->
                    case model.importResponse of
                        Remote.Loading ->
                            ( [], spinner, viewRemoteError model.importResponse )

                        Remote.Success importDetail ->
                            if importDetail.status == Status.Failed || importDetail.status == Status.Cancelled then
                                ( [ onClick CreateDataSet ], i [ class "fa fa-upload mr5" ] [ text "Import" ], viewMessagesAsError importDetail.messages )
                            else
                                ( [], spinner, viewMessagesAsError importDetail.messages )

                        _ ->
                            ( [ onClick CreateDataSet ], i [ class "fa fa-upload mr5" ] [ text "Import" ], viewRemoteError model.importResponse )

        addButton =
            button (class "btn" :: buttonAttributes) [ createButtonContent ]
    in
    div [ class "col-sm-12" ]
        [ div [ id "review" ] <| viewEntryReview model
        , hr [] []
        , h3 [ class "mt0" ] [ text "Do you want to specify a key?" ]
        , div [ class "col-sm-4" ]
            [ div [ class "form-group" ]
                [ label [] [ text "Key" ]
                , input [ class "form-control", placeholder "(Optional)", value model.key ] []
                ]
            , div [ class "form-group" ]
                [ addButton ]
            , errorDisplay
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ h5 [] [ text "Why choosing a key is important" ]
                , p [] [ text "Tell them why it matters." ]
                ]
            ]
        ]


viewEntryReview : Model -> List (Html Msg)
viewEntryReview model =
    let
        dataSetName =
            ( "DataSet Name", model.name )

        properties =
            case model.activeTab of
                FileUploadTab fileUpload ->
                    [ ( "Filename", fileUpload.fileName ) ]

                UrlImportTab urlImport ->
                    [ ( "Url", urlImport.importUrl ) ]
    in
    dataSetName
        :: properties
        |> List.map viewReviewPropertyValue


viewReviewPropertyValue : ( String, String ) -> Html Msg
viewReviewPropertyValue ( name, value ) =
    div [ class "form-group col-sm-4" ]
        [ p [] [ text name ]
        , h6 [] [ text value ]
        ]


viewTabControl : Model -> Html Msg
viewTabControl model =
    let
        tabHeaders =
            [ li [ classList [ ( "active", model.activeTabName == "Upload" ) ] ] [ a [ onClick (ChangeTab (FileUploadTab initFileUploadTab => "Upload")) ] [ text "Upload" ] ]
            , li [ classList [ ( "active", model.activeTabName == "URL" ) ] ] [ a [ onClick (ChangeTab (UrlImportTab initImportTab => "URL")) ] [ text "Import from URL" ] ]
            ]
    in
    ul [ class "nav nav-tabs", attribute "role" "tablist" ]
        tabHeaders


viewTabContent : Model -> Html Msg
viewTabContent model =
    let
        content =
            case model.activeTab of
                FileUploadTab fileUploadEntry ->
                    viewUploadTab fileUploadEntry

                UrlImportTab urlImportEntry ->
                    viewImportUrlTab urlImportEntry
    in
    div [ class "tab-content" ]
        [ div [ class "tab-pane active" ]
            [ content ]
        ]


viewUploadTab : FileUploadEntry -> Html Msg
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
                    [ id "upload-dataset"
                    , class "upload-file"
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
                [ h5 [] [ text "How to upload a CSV/JSON" ]
                , p [] [ text "Instructions go here." ]
                ]
            ]
        ]


viewImportUrlTab : UrlImportEntry -> Html Msg
viewImportUrlTab model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-8" ]
                [ label [] [ text "File URL" ]
                , input [ class "form-control", onInput <| \c -> TabMsg (ImportUrlInputChange c), value model.importUrl ] []
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ h5 [] [ text "How to import from a URL" ]
                , p [] [ text "URL instructions go here" ]
                ]
            ]
        ]


viewPasteInTab : Model -> Html Msg
viewPasteInTab model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            []
        , div [ class "col-sm-6" ]
            []
        ]


configWizard : WizardConfig Msg
configWizard =
    { nextMessage = NextStep
    , prevMessage = PrevStep
    }
