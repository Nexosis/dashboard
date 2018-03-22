module Page.DataSetAdd exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes exposing (Route)
import Data.Config exposing (Config)
import Data.Context exposing (ContextModel)
import Data.DataFormat as DataFormat
import Data.DataSet
import Data.File as File
import Data.Import
import Data.Status as Status
import Data.Ziplist as Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onBlur, onClick, onInput)
import Http
import Json.Decode exposing (succeed)
import List.Extra as List
import Navigation
import Page.Helpers exposing (explainer)
import Ports exposing (fileContentRead, uploadFileSelected)
import Regex
import RemoteData as Remote
import Request.DataSet exposing (PutUploadRequest, put)
import Request.Import exposing (PostUrlRequest)
import Request.Log as Log
import String.Verify exposing (notBlank)
import Task exposing (Task)
import Util exposing ((=>), delayTask, spinner, unwrapErrors)
import Verify exposing (Validator)
import View.Breadcrumb as Breadcrumb
import View.Error exposing (viewFieldError, viewMessagesAsError, viewRemoteError)
import View.Extra exposing (viewIf, viewIfElements)
import View.Wizard as Wizard exposing (WizardConfig, viewButtons)


type alias Model =
    { steps : Ziplist Step
    , name : String
    , key : Maybe String
    , tabs : Ziplist ( Tab, String )
    , uploadResponse : Remote.WebData ()
    , importResponse : Remote.WebData Data.Import.ImportDetail
    , errors : List FieldError
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


type AddDataSetRequest
    = PutUpload PutUploadRequest
    | ImportUrl PostUrlRequest


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


initTabs : Ziplist ( Tab, String )
initTabs =
    Ziplist.create []
        ( FileUploadTab <| initFileUploadTab, "Upload" )
        [ ( UrlImportTab <| initImportTab, "Import from URL" )
        ]


init : Config -> ( Model, Cmd Msg )
init config =
    let
        steps =
            Ziplist.create [] ChooseUploadType [ SetKey ]
    in
    Model
        steps
        ""
        Nothing
        initTabs
        Remote.NotAsked
        Remote.NotAsked
        []
        => Cmd.none



-- Validation


type Field
    = DataSetNameField
    | UrlField
    | FileSelectionField


type alias FieldError =
    ( Field, String )


validateModel : Model -> Result (List FieldError) AddDataSetRequest
validateModel model =
    case model.tabs.current of
        ( FileUploadTab fileUploadEntry, _ ) ->
            validateFileUploadModel model fileUploadEntry
                |> Result.map PutUpload

        ( UrlImportTab urlImportEntry, _ ) ->
            validateUrlImportModel model urlImportEntry
                |> Result.map ImportUrl


validateFileUploadModel : Model -> Validator FieldError FileUploadEntry PutUploadRequest
validateFileUploadModel model =
    Verify.ok PutUploadRequest
        |> Verify.verify (always model.name) (notBlank (DataSetNameField => "DataSet name required."))
        |> Verify.verify .fileContent (notBlank (FileSelectionField => "Choose a file to upload."))
        |> Verify.verify .fileUploadType (verifyFileType (FileSelectionField => "Upload a CSV or JSON file."))


validateUrlImportModel : Model -> Validator FieldError UrlImportEntry PostUrlRequest
validateUrlImportModel model =
    Verify.ok PostUrlRequest
        |> Verify.verify (always model.name) (notBlank (DataSetNameField => "DataSet name required."))
        |> Verify.verify .importUrl (verifyRegex urlRegex (UrlField => "Enter a valid url."))
        |> Verify.keep (always model.key)


verifyFileType : error -> Validator error DataFormat.DataFormat String
verifyFileType error input =
    case input of
        DataFormat.Other ->
            Err [ error ]

        _ ->
            Ok <| DataFormat.dataFormatToContentType input


verifyRegex : Regex.Regex -> error -> Validator error String String
verifyRegex regex error input =
    if Regex.contains regex input then
        Ok input
    else
        Err [ error ]


urlRegex : Regex.Regex
urlRegex =
    -- Just do a simple check for something that starts with http or https, and doesn't have spaces.
    Regex.regex "^(http|https):\\/\\/[^ \"]+$"


perStepValidations : List ( Step, Model -> List FieldError )
perStepValidations =
    [ ( ChooseUploadType, validateModel >> unwrapErrors ) ]


configWizard : WizardConfig Step FieldError Msg Model AddDataSetRequest
configWizard =
    { nextMessage = NextStep
    , prevMessage = PrevStep
    , stepValidation = perStepValidations
    , finishedButton = finalStepButton
    , finishedValidation = validateModel
    , finishedMsg = CreateDataSet
    }



-- Update


type Msg
    = ChangeName String
    | ChangeKey String
    | NextStep
    | InputBlur
    | PrevStep
    | ChangeTab String
    | FileSelected
    | TabMsg TabMsg
    | CreateDataSet AddDataSetRequest
    | UploadDataSetResponse (Remote.WebData ())
    | ImportResponse (Remote.WebData Data.Import.ImportDetail)


type TabMsg
    = FileContentRead Json.Decode.Value
    | ImportUrlInputChange String


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case ( model.steps.current, msg ) of
        ( ChooseUploadType, ChangeName name ) ->
            { model | name = name } => Cmd.none

        ( ChooseUploadType, ChangeTab tabName ) ->
            let
                newTabs =
                    model.tabs
                        |> Ziplist.find (\( _, name ) -> name == tabName)
                        |> Maybe.withDefault model.tabs
            in
            { model | tabs = newTabs } => Ports.prismHighlight ()

        ( ChooseUploadType, FileSelected ) ->
            model => Ports.uploadFileSelected "upload-dataset"

        ( ChooseUploadType, TabMsg tabMsg ) ->
            updateTabContents model tabMsg => Cmd.none

        ( SetKey, ChangeKey key ) ->
            let
                keyValue =
                    if String.isEmpty key then
                        Nothing
                    else
                        Just key
            in
            { model | key = keyValue } => Cmd.none

        ( SetKey, CreateDataSet createRequest ) ->
            case createRequest of
                PutUpload request ->
                    let
                        putRequest =
                            put context.config request
                                |> Remote.sendRequest
                                |> Cmd.map UploadDataSetResponse
                    in
                    { model | uploadResponse = Remote.Loading } => putRequest

                ImportUrl request ->
                    let
                        importRequest =
                            Request.Import.postUrl context.config request
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
                                delayAndRecheckImport context.config importDetail.importId

                        _ ->
                            Cmd.none
            in
            { model | importResponse = result } => cmd

        ( _, NextStep ) ->
            let
                errors =
                    validateStep model
            in
            if errors == [] then
                { model | steps = Ziplist.advance model.steps } => Cmd.none
            else
                { model | errors = errors } => Cmd.none

        ( _, PrevStep ) ->
            { model | steps = Ziplist.rewind model.steps } => Cmd.none

        ( _, InputBlur ) ->
            if model.errors /= [] then
                { model | errors = validateStep model } => Cmd.none
            else
                model => Cmd.none

        _ ->
            model => Cmd.none


validateStep : Model -> List FieldError
validateStep model =
    let
        stepValidation =
            perStepValidations
                |> List.find (\s -> Tuple.first s |> (==) model.steps.current)
                |> Maybe.map Tuple.second
                |> Maybe.withDefault (\_ -> [])
    in
    stepValidation model


delayAndRecheckImport : Config -> String -> Cmd Msg
delayAndRecheckImport config importId =
    delayTask 2
        |> Task.andThen (\_ -> Request.Import.get config importId |> Http.toTask)
        |> Remote.asCmd
        |> Cmd.map ImportResponse


updateTabContents : Model -> TabMsg -> Model
updateTabContents model msg =
    let
        tabModel =
            case ( model.tabs.current, msg ) of
                ( ( FileUploadTab fileUploadEntry, id ), FileContentRead readResult ) ->
                    let
                        readStatus =
                            Json.Decode.decodeValue File.fileReadStatusDecoder readResult
                                |> Result.withDefault File.ReadError

                        m =
                            case readStatus of
                                File.Success fileName content ->
                                    case DataFormat.filenameToType fileName of
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
                    in
                    ( FileUploadTab m, id )

                ( ( UrlImportTab urlTab, id ), ImportUrlInputChange urlEntry ) ->
                    let
                        urlImportModel =
                            { urlTab | importUrl = urlEntry }
                    in
                    ( UrlImportTab urlImportModel, id )

                ( _, _ ) ->
                    model.tabs.current

        tabs =
            model.tabs

        updatedTabs =
            { tabs | current = tabModel }
    in
    { model | tabs = updatedTabs }


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead <| \c -> TabMsg (FileContentRead c)


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ id "page-header", class "row" ]
            [ Breadcrumb.list
            , div [ class "col-sm-6" ] [ h2 [] [ text "Add DataSet" ] ]
            ]
        , div [ class "row" ]
            [ case model.steps.current of
                ChooseUploadType ->
                    viewChooseUploadType context model

                SetKey ->
                    viewSetKey context.config model
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12 right" ]
                [ viewButtons configWizard
                    model
                    model.steps
                    (Remote.isLoading model.importResponse || Remote.isLoading model.uploadResponse)
                    (model.errors == [])
                ]
            ]
        ]


viewChooseUploadType : ContextModel -> Model -> Html Msg
viewChooseUploadType context model =
    div []
        [ div [ class "col-sm-12 mb20 session-step" ]
            [ div [ class "col-sm-6 pl0" ] [ h3 [] [ text "Choose Upload type" ] ]
            , div [ class "col-sm-6 right" ]
                [ viewButtons configWizard
                    model
                    model.steps
                    (Remote.isLoading model.importResponse || Remote.isLoading model.uploadResponse)
                    (model.errors == [])
                ]
            ]
        , div
            [ class "col-sm-12" ]
            [ div [ class "form-group col-sm-4" ]
                [ label [] [ text "DataSet Name" ]
                , input [ type_ "text", class "form-control", onInput ChangeName, onBlur InputBlur, value model.name ] []
                , viewFieldError model.errors DataSetNameField
                ]
            ]
        , div [ class "col-sm-12" ]
            [ viewTabControl model
            , viewTabContent context model
            ]
        ]


finalStepButton : Model -> Wizard.HtmlDetails Msg
finalStepButton model =
    let
        buttonContent =
            case model.tabs.current of
                ( FileUploadTab _, _ ) ->
                    case model.uploadResponse of
                        Remote.Loading ->
                            [ spinner ]

                        _ ->
                            [ text "Create DataSet" ]

                _ ->
                    case model.importResponse of
                        Remote.Loading ->
                            [ spinner ]

                        Remote.Success importDetail ->
                            if importDetail.status == Status.Failed || importDetail.status == Status.Cancelled then
                                [ i [ class "fa fa-upload mr5" ] [], text "Import" ]
                            else
                                [ spinner ]

                        _ ->
                            [ i [ class "fa fa-upload mr5" ] [], text "Import" ]
    in
    Wizard.HtmlDetails [] buttonContent


viewSetKey : Config -> Model -> Html Msg
viewSetKey config model =
    let
        errorDisplay =
            case model.tabs.current of
                ( FileUploadTab _, _ ) ->
                    viewRemoteError model.uploadResponse

                _ ->
                    case model.importResponse of
                        Remote.Success importDetail ->
                            viewMessagesAsError importDetail.messages

                        _ ->
                            viewRemoteError model.importResponse
    in
    div []
        [ div [ class "col-sm-12" ]
            [ div [ class "col-sm-8 pl0" ]
                [ div [ id "review" ] <| viewEntryReview model
                ]
            , div
                [ class "col-sm-4 right" ]
                [ viewButtons configWizard model model.steps (Remote.isLoading model.importResponse || Remote.isLoading model.uploadResponse) (model.errors == []) ]
            ]
        , div
            [ class "col-sm-12" ]
            [ hr []
                []
            , h3
                [ class "mt0" ]
                [ text "Do you want to specify a key?" ]
            , div [ class "col-sm-4" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Key" ]
                    , input [ type_ "text", class "form-control", placeholder "(Optional)", value <| Maybe.withDefault "" model.key, onInput ChangeKey ] []
                    ]
                , errorDisplay
                ]
            , div [ class "col-sm-6 col-sm-offset-2" ]
                [ div [ class "alert alert-info" ]
                    [ explainer config "why_choose_key"
                    ]
                ]
            ]
        ]


viewEntryReview : Model -> List (Html Msg)
viewEntryReview model =
    let
        dataSetName =
            ( "DataSet Name", model.name )

        properties =
            case model.tabs.current of
                ( FileUploadTab fileUpload, _ ) ->
                    [ ( "Filename", fileUpload.fileName ) ]

                ( UrlImportTab urlImport, _ ) ->
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
            (model.tabs.previous |> List.map viewInactiveTab)
                ++ [ viewActiveTab model.tabs.current ]
                ++ (model.tabs.next |> List.map viewInactiveTab)
    in
    ul [ class "nav nav-tabs", attribute "role" "tablist" ]
        tabHeaders


viewInactiveTab : ( Tab, String ) -> Html Msg
viewInactiveTab ( _, tabText ) =
    li [] [ a [ attribute "role" "button", onClick (ChangeTab tabText) ] [ text tabText ] ]


viewActiveTab : ( Tab, String ) -> Html Msg
viewActiveTab ( _, tabText ) =
    li [ class "active" ] [ a [ attribute "role" "button", onClick (ChangeTab tabText) ] [ text tabText ] ]


viewTabContent : ContextModel -> Model -> Html Msg
viewTabContent context model =
    let
        content =
            case model.tabs.current of
                ( FileUploadTab fileUploadEntry, _ ) ->
                    viewUploadTab context.config fileUploadEntry model

                ( UrlImportTab urlImportEntry, _ ) ->
                    viewImportUrlTab context.config urlImportEntry model
    in
    div [ class "tab-content" ]
        [ div [ class "tab-pane active" ]
            [ content ]
        ]


viewUploadTab : Config -> FileUploadEntry -> Model -> Html Msg
viewUploadTab config tabModel model =
    let
        uploadButtonText =
            if String.isEmpty tabModel.fileName then
                "Select your file"
            else
                tabModel.fileName
    in
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-8" ]
                [ input
                    [ id "upload-dataset"
                    , type_ "file"
                    , on "change" (succeed FileSelected)
                    , onBlur InputBlur
                    ]
                    []
                , label [ for "upload-dataset" ] [ text uploadButtonText ]
                , viewFieldError model.errors FileSelectionField
                , viewIf (\() -> div [ class "alert alert-danger" ] [ text "An error occurred when uploading the file.  Please ensure it is a valid JSON or CSV file and try again." ]) tabModel.fileUploadErrorOccurred
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ explainer config "how_upload_csv"
                ]
            ]
        ]


viewImportUrlTab : Config -> UrlImportEntry -> Model -> Html Msg
viewImportUrlTab config tabModel model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-8" ]
                [ label [] [ text "File URL" ]
                , input [ type_ "text", class "form-control", onInput <| \c -> TabMsg (ImportUrlInputChange c), value tabModel.importUrl, onBlur InputBlur ] []
                , viewFieldError model.errors UrlField
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ explainer config "how_upload_url"
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
