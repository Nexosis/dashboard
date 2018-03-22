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
import Request.DataSet exposing (PutUploadRequest, createDataSetWithKey, put)
import Request.Import exposing (PostS3Request, PostUrlRequest)
import Request.Log as Log
import String.Verify exposing (notBlank)
import Task exposing (Task)
import Util exposing ((=>), delayTask, spinner, unwrapErrors)
import Verify exposing (Validator, keep)
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


type alias S3ImportEntry =
    { bucket : String
    , path : String
    , region : Maybe String
    , accessKeyId : Maybe String
    , secretAccessKey : Maybe String
    }


type Step
    = ChooseUploadType
    | SetKey


type Tab
    = FileUploadTab FileUploadEntry
    | UrlImportTab UrlImportEntry
    | S3ImportTab S3ImportEntry


type AddDataSetRequest
    = PutUpload PutUploadRequest
    | ImportUrl PostUrlRequest
    | ImportS3 PostS3Request


initFileUploadTab : FileUploadEntry
initFileUploadTab =
    { fileContent = ""
    , fileName = ""
    , fileUploadType = DataFormat.Other
    , fileUploadErrorOccurred = False
    }


initImportUrlTab : UrlImportEntry
initImportUrlTab =
    { importUrl = "" }


initImportS3Tab : S3ImportEntry
initImportS3Tab =
    { path = "", bucket = "", region = Nothing, accessKeyId = Nothing, secretAccessKey = Nothing }


initTabs : Ziplist ( Tab, String )
initTabs =
    Ziplist.create []
        ( FileUploadTab <| initFileUploadTab, "Upload" )
        [ ( UrlImportTab <| initImportUrlTab, "Import from URL" )
        , ( S3ImportTab <| initImportS3Tab, "Import from AWS S3" )
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
    | PathField
    | BucketField
    | RegionField
    | AccessTokenIdField
    | SecretAccessKeyField


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

        ( S3ImportTab s3ImportEntry, _ ) ->
            validateS3ImportModel model s3ImportEntry
                |> Result.map ImportS3


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


validateS3ImportModel : Model -> Validator FieldError S3ImportEntry PostS3Request
validateS3ImportModel model =
    Verify.ok PostS3Request
        |> Verify.verify (always model.name) (notBlank (DataSetNameField => "DataSet name required."))
        |> Verify.verify .bucket (notBlank (BucketField => "Bucket required."))
        |> Verify.verify .path (notBlank (PathField => "Path required."))
        |> Verify.keep .region
        |> Verify.keep .accessKeyId
        |> Verify.keep .secretAccessKey


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
    | S3PathChange String
    | S3BucketChange String
    | S3RegionChange String
    | S3AccessKeyIdChange String
    | S3SecretAccessKeyChange String


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
                        setKeyRequest =
                            case model.key of
                                Just key ->
                                    createDataSetWithKey context.config request.name key
                                        |> Http.toTask

                                Nothing ->
                                    Task.succeed ()

                        putDataRequest =
                            put context.config request
                                |> Http.toTask

                        putRequest =
                            setKeyRequest
                                |> Task.andThen (always putDataRequest)
                                |> Remote.asCmd
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

                ImportS3 request ->
                    let
                        importRequest =
                            Request.Import.postS3 context.config request
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

                ( ( S3ImportTab s3Tab, id ), changeMsg ) ->
                    case changeMsg of
                        S3PathChange path ->
                            ( S3ImportTab { s3Tab | path = path }, id )

                        S3BucketChange bucket ->
                            ( S3ImportTab { s3Tab | bucket = bucket }, id )

                        S3RegionChange region ->
                            ( S3ImportTab { s3Tab | region = Just <| region }, id )

                        S3AccessKeyIdChange accessKeyId ->
                            ( S3ImportTab { s3Tab | accessKeyId = Just <| accessKeyId }, id )

                        S3SecretAccessKeyChange secret ->
                            ( S3ImportTab { s3Tab | secretAccessKey = Just <| secret }, id )

                        _ ->
                            ( S3ImportTab s3Tab, id )

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

                ( S3ImportTab s3Import, _ ) ->
                    [ ( "Path", s3Import.path )
                    , ( "Bucket", s3Import.bucket )
                    , ( "Region", Maybe.withDefault "" s3Import.region )
                    , ( "Access Key Id", Maybe.withDefault "" s3Import.accessKeyId )
                    , ( "Secret Access Key", Maybe.withDefault "" s3Import.secretAccessKey )
                    ]
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

                ( S3ImportTab s3ImportEntry, _ ) ->
                    viewImportS3Tab context.config s3ImportEntry model
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


viewImportS3Tab : Config -> S3ImportEntry -> Model -> Html Msg
viewImportS3Tab config tabModel model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-8" ]
                [ label [] [ text "Bucket Name" ]
                , input [ class "form-control", onInput <| \c -> TabMsg (S3BucketChange c), value tabModel.bucket, onBlur InputBlur ] []
                , viewFieldError model.errors BucketField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "File Path" ]
                , input [ class "form-control", onInput <| \c -> TabMsg (S3PathChange c), value tabModel.path, onBlur InputBlur ] []
                , viewFieldError model.errors PathField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "AWS Region" ]
                , input [ class "form-control", onInput <| \c -> TabMsg (S3RegionChange c), value <| Maybe.withDefault "" tabModel.region, onBlur InputBlur ] []
                , viewFieldError model.errors RegionField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "Access Key Id" ]
                , input [ class "form-control", onInput <| \c -> TabMsg (S3AccessKeyIdChange c), value <| Maybe.withDefault "" tabModel.accessKeyId, onBlur InputBlur ] []
                , viewFieldError model.errors AccessTokenIdField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "Secret Access Key" ]
                , input [ class "form-control", onInput <| \c -> TabMsg (S3SecretAccessKeyChange c), value <| Maybe.withDefault "" tabModel.secretAccessKey, onBlur InputBlur ] []
                , viewFieldError model.errors SecretAccessKeyField
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ explainer config "how_upload_s3"
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
