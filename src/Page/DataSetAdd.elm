module Page.DataSetAdd exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes exposing (Route)
import Csv
import Data.Config exposing (Config)
import Data.Context as AppContext exposing (ContextModel)
import Data.DataFormat as DataFormat
import Data.DataSet
import Data.File as File
import Data.Import
import Data.Response as Response exposing (Quotas, maxSize)
import Data.Status as Status
import Data.Ziplist as Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onBlur, onClick, onInput)
import Http
import Json.Decode exposing (decodeString, succeed)
import List.Extra as ListEx
import Navigation
import Page.Helpers exposing (explainer)
import Ports exposing (fileContentRead, uploadFileSelected)
import Regex
import RemoteData as Remote
import Request.DataSet exposing (PutUploadRequest, UploadData(..), createDataSetWithKey, put)
import Request.Import exposing (PostAzureRequest, PostS3Request, PostUrlRequest)
import Request.Log as Log
import String.Verify exposing (notBlank)
import Task exposing (Task)
import Util exposing ((=>), dataSizeWithCustomKSize, dataSizeWithSuffix, delayTask, formatDisplayName, formatDisplayNameWithWidth, spinner, unwrapErrors)
import Verify exposing (Validator, andThen, keep)
import View.Breadcrumb as Breadcrumb
import View.Error exposing (viewFieldError, viewMessagesAsError, viewRemoteError)
import View.Extra exposing (viewIf, viewIfElements, viewJust)
import View.Wizard as Wizard exposing (WizardConfig, viewButtons)


type alias Model =
    { steps : Ziplist Step
    , name : String
    , key : Maybe String
    , tabs : Ziplist ( Tab, String )
    , uploadResponse : Remote.WebData ()
    , importResponse : Remote.WebData Data.Import.ImportDetail
    , awsRegions : AwsRegions
    , errors : List FieldError
    , uploadPartsTotal : Int
    , uploadedParts : Int
    }


type alias FileUploadEntry =
    { content : String
    , fileName : String
    , contentType : DataFormat.DataFormat
    , fileUploadErrorOccurred : Maybe File.FileUploadErrorType
    }


type alias DirectDataEntry =
    { content : String
    , contentType : DataFormat.DataFormat
    }


type alias UrlImportEntry =
    { importUrl : String }


type alias AwsRegions =
    { regions : List ( String, String )
    }


type alias S3ImportEntry =
    { bucket : String
    , path : String
    , region : Maybe String
    , accessKeyId : Maybe String
    , secretAccessKey : Maybe String
    }


type alias AzureImportEntry =
    { connectionString : String
    , container : String
    , blob : String
    }


type Step
    = ChooseUploadType
    | SetKey


type Tab
    = FileUploadTab FileUploadEntry
    | DirectDataTab DirectDataEntry
    | UrlImportTab UrlImportEntry
    | S3ImportTab S3ImportEntry
    | AzureImportTab AzureImportEntry


type AddDataSetRequest
    = PutUpload PutUploadRequest
    | ImportUrl PostUrlRequest
    | ImportS3 PostS3Request
    | ImportAzure PostAzureRequest


initFileUploadTab : FileUploadEntry
initFileUploadTab =
    { content = ""
    , fileName = ""
    , contentType = DataFormat.Other
    , fileUploadErrorOccurred = Nothing
    }


initDirectDataTab : DirectDataEntry
initDirectDataTab =
    { content = ""
    , contentType = DataFormat.Other
    }


initAzureImportTab : AzureImportEntry
initAzureImportTab =
    { connectionString = "", container = "", blob = "" }


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
        [ ( DirectDataTab <| initDirectDataTab, "Enter" )
        , ( UrlImportTab <| initImportUrlTab, "Import from URL" )
        , ( S3ImportTab <| initImportS3Tab, "Import from AWS S3" )
        , ( AzureImportTab <| initAzureImportTab, "Import from Azure Blob Storage" )
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
        listAwsRegions
        []
        0
        0
        => Cmd.none


listAwsRegions : AwsRegions
listAwsRegions =
    AwsRegions
        [ ( "us-east-1", "US East (N. Virginia)" )
        , ( "us-east-2", "US East (Ohio)" )
        , ( "us-west-1", "US West (N. California)" )
        , ( "us-west-2", "US West (Oregon)" )
        , ( "ca-central-1", "Canada (Central)" )
        , ( "eu-central-1", "EU (Frankfurt)" )
        , ( "eu-west-1", "EU (Ireland)" )
        , ( "eu-west-2", "EU (London)" )
        , ( "eu-west-3", "EU (Paris)" )
        , ( "ap-northeast-1", "Asia Pacific (Tokyo)" )
        , ( "ap-northeast-2", "Asia Pacific (Seoul)" )
        , ( "ap-northeast-3", "Asia Pacific (Osaka-Local)" )
        , ( "ap-southeast-1", "Asia Pacific (Singapore)" )
        , ( "ap-southeast-2", "Asia Pacific (Sydney)" )
        , ( "ap-south-1", "Asia Pacific (Mumbai)" )
        , ( "sa-east-1", "South America (São Paulo)" )
        ]



-- Validation


type Field
    = DataSetNameField
    | UrlField
    | FileSelectionField
    | DataField
    | AwsPathField
    | AwsBucketField
    | AwsRegionField
    | AwsAccessTokenIdField
    | AwsSecretAccessKeyField
    | AzureConnectionStringField
    | AzureContainerField
    | AzureBlobField


type alias FieldError =
    ( Field, String )


validateModel : ContextModel -> Model -> Result (List FieldError) AddDataSetRequest
validateModel context model =
    case model.tabs.current of
        ( FileUploadTab fileUploadEntry, _ ) ->
            validateFileUploadModel model context fileUploadEntry
                |> Result.map PutUpload

        ( DirectDataTab directDataEntry, _ ) ->
            validateDirectDataModel model context directDataEntry
                |> Result.map PutUpload

        ( UrlImportTab urlImportEntry, _ ) ->
            validateUrlImportModel model urlImportEntry
                |> Result.map ImportUrl

        ( S3ImportTab s3ImportEntry, _ ) ->
            validateS3ImportModel model s3ImportEntry
                |> Result.map ImportS3

        ( AzureImportTab azureImportEntry, _ ) ->
            validateAzureImportModel model azureImportEntry
                |> Result.map ImportAzure


validateFileUploadModel : Model -> ContextModel -> Validator FieldError FileUploadEntry PutUploadRequest
validateFileUploadModel model context =
    Verify.ok PutUploadRequest
        |> Verify.verify (always model.name) (notBlank (DataSetNameField => "DataSet name required."))
        |> Verify.custom (verifyDataContent context FileSelectionField)


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
        |> Verify.verify .bucket (notBlank (AwsBucketField => "Bucket required."))
        |> Verify.verify .path (notBlank (AwsPathField => "Path required."))
        |> Verify.keep .region
        |> Verify.keep .accessKeyId
        |> Verify.keep .secretAccessKey


validateAzureImportModel : Model -> Validator FieldError AzureImportEntry PostAzureRequest
validateAzureImportModel model =
    Verify.ok PostAzureRequest
        |> Verify.verify (always model.name) (notBlank (DataSetNameField => "DataSet name required."))
        |> Verify.verify .connectionString (notBlank (AzureConnectionStringField => "Connection String required."))
        |> Verify.verify .container (notBlank (AzureContainerField => "Container required."))
        |> Verify.verify .blob (notBlank (AzureBlobField => "Blob required."))


validateDirectDataModel : Model -> ContextModel -> Validator FieldError DirectDataEntry PutUploadRequest
validateDirectDataModel model context =
    Verify.ok PutUploadRequest
        |> Verify.verify (always model.name) (notBlank (DataSetNameField => "DataSet name required."))
        |> Verify.custom (verifyDataContent context DataField)


verifyFileType : error -> Validator error DataFormat.DataFormat DataFormat.DataFormat
verifyFileType error input =
    case input of
        DataFormat.Other ->
            Err [ error ]

        _ ->
            Ok <| input


verifyDataContent : ContextModel -> field -> Validator ( field, String ) { c | contentType : DataFormat.DataFormat, content : String } UploadData
verifyDataContent context field input =
    let
        tooBig =
            (String.length input.content * 2) > maxSize context.quotas

        asCsv content =
            String.lines content
                |> List.tail
                |> Maybe.withDefault []
                |> File.CsvData (parseCsv content |> .headers)
                |> Csv
                |> Ok

        parseCsv csv =
            String.lines csv
                |> List.take 100
                |> String.join "\x0D\n"
                |> Csv.parse
    in
    case tooBig of
        True ->
            Err [ field => ("Data must be less than " ++ (maxSize context.quotas |> dataSizeWithSuffix) ++ " in size") ]

        False ->
            case input.contentType of
                DataFormat.Json ->
                    parseJson field input.content

                DataFormat.Csv ->
                    asCsv input.content

                _ ->
                    Err <| [ field => "Invalid content type" ]


parseJson : field -> String -> Result (List ( field, String )) UploadData
parseJson field content =
    decodeString File.jsonDataDecoder content
        |> Result.map (\d -> Json d)
        |> Result.mapError (\e -> [ field => e ])


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


perStepValidations : ContextModel -> List ( Step, Model -> List FieldError )
perStepValidations ctx =
    [ ( ChooseUploadType, validateModel ctx >> unwrapErrors ) ]


configWizard : ContextModel -> Bool -> WizardConfig Step FieldError Msg Model AddDataSetRequest
configWizard ctx isBottom =
    { nextMessage = NextStep
    , prevMessage = PrevStep
    , stepValidation = perStepValidations ctx
    , finishedButton = finalStepButton isBottom
    , finishedValidation = validateModel ctx
    , finishedMsg = CreateDataSet
    , customLoading = Just (waiting isBottom)
    }


waiting : Bool -> Model -> Html Msg
waiting isBottom model =
    let
        width =
            (model.uploadedParts * 100) // model.uploadPartsTotal |> toString
    in
    if Remote.isLoading model.uploadResponse && not isBottom then
        div [ class "progress" ]
            [ div [ class "progress-bar", attribute "role" "progressbar", attribute "aria-valuemin" "0", attribute "aria-valuemax" (toString model.uploadPartsTotal), attribute "aria-valuenow" width, attribute "style" ("width:" ++ width ++ "%") ]
                [ span [] [ text (width ++ "%") ]
                ]
            ]
    else if Remote.isLoading model.importResponse || Remote.isSuccess model.importResponse then
        div [ class "btn btn-danger", disabled True ] [ text "Importing... ", spinner ]
    else
        div [] []



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
    | UploadDataSetParts ( List (Task Http.Error ()), Remote.WebData () )
    | ImportResponse (Remote.WebData Data.Import.ImportDetail)


type TabMsg
    = FileContentRead Json.Decode.Value
    | DataChange String
    | ImportUrlInputChange String
    | S3PathChange String
    | S3BucketChange String
    | S3RegionChange String
    | S3AccessKeyIdChange String
    | S3SecretAccessKeyChange String
    | AzureConnectionStringChange String
    | AzureContainerChange String
    | AzureBlobChange String


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    let
        next requests =
            case List.head requests of
                Just r ->
                    let
                        toCmd r =
                            UploadDataSetParts ( Maybe.withDefault [] (List.tail requests), r )
                    in
                    r |> Remote.asCmd |> Cmd.map toCmd

                _ ->
                    Cmd.none
    in
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
            { model | errors = [] } => Ports.uploadFileSelected ( "upload-dataset", maxSize context.quotas )

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
            let
                setKeyRequest name =
                    case model.key of
                        Just key ->
                            createDataSetWithKey context.config name key
                                |> Http.toTask

                        Nothing ->
                            Task.succeed ()
            in
            case createRequest of
                PutUpload request ->
                    let
                        putDataRequests : List (Task Http.Error ())
                        putDataRequests =
                            put context.config request
                                |> List.map Http.toTask

                        putRequests : List (Task Http.Error ())
                        putRequests =
                            [ setKeyRequest request.name ] ++ putDataRequests
                    in
                    { model | uploadResponse = Remote.Loading, uploadPartsTotal = List.length putRequests } => next putRequests

                ImportUrl request ->
                    let
                        doImport =
                            Request.Import.postUrl context.config request
                                |> Http.toTask

                        importRequest =
                            setKeyRequest request.dataSetName
                                |> Task.andThen (always doImport)
                                |> Remote.asCmd
                                |> Cmd.map ImportResponse
                    in
                    { model | importResponse = Remote.Loading } => importRequest

                ImportS3 request ->
                    let
                        doImport =
                            Request.Import.postS3 context.config request
                                |> Http.toTask

                        importRequest =
                            setKeyRequest request.dataSetName
                                |> Task.andThen (always doImport)
                                |> Remote.asCmd
                                |> Cmd.map ImportResponse
                    in
                    { model | importResponse = Remote.Loading } => importRequest

                ImportAzure request ->
                    let
                        doImport =
                            Request.Import.postAzure context.config request
                                |> Http.toTask

                        importRequest =
                            setKeyRequest request.dataSetName
                                |> Task.andThen (always doImport)
                                |> Remote.asCmd
                                |> Cmd.map ImportResponse
                    in
                    { model | importResponse = Remote.Loading } => importRequest

        ( _, UploadDataSetParts ( rest, curr ) ) ->
            case curr of
                Remote.Success _ ->
                    case rest of
                        [] ->
                            model => (Navigation.load <| AppRoutes.routeToString (AppRoutes.DataSetDetail <| Data.DataSet.toDataSetName model.name))

                        _ ->
                            { model | uploadedParts = model.uploadedParts + 1 } => next rest

                Remote.Failure err ->
                    { model | uploadResponse = curr } => Log.logHttpError err

                _ ->
                    model => Cmd.none

        ( _, ImportResponse result ) ->
            let
                ( importResult, cmd ) =
                    case result of
                        Remote.Success importDetail ->
                            if importDetail.status == Status.Completed then
                                result => AppRoutes.newUrl (AppRoutes.DataSetDetail importDetail.dataSetName)
                            else if importDetail.status == Status.Cancelled || importDetail.status == Status.Failed then
                                result => Cmd.none
                            else
                                Remote.Loading => delayAndRecheckImport context.config importDetail.importId

                        _ ->
                            result => Cmd.none
            in
            { model | importResponse = importResult } => cmd

        ( _, NextStep ) ->
            let
                errors =
                    validateStep context model
            in
            if errors == [] then
                { model | steps = Ziplist.advance model.steps } => Cmd.none
            else
                { model | errors = errors } => Cmd.none

        ( _, PrevStep ) ->
            { model | steps = Ziplist.rewind model.steps } => Cmd.none

        ( _, InputBlur ) ->
            if model.errors /= [] then
                { model | errors = validateStep context model } => Cmd.none
            else
                model => Cmd.none

        _ ->
            model => Cmd.none


validateStep : ContextModel -> Model -> List FieldError
validateStep context model =
    let
        stepValidation =
            perStepValidations context
                |> ListEx.find (\s -> Tuple.first s |> (==) model.steps.current)
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
                                |> Result.withDefault (File.ReadError File.UnknownError)

                        m =
                            case readStatus of
                                File.Success fileName content ->
                                    case DataFormat.filenameToType fileName of
                                        DataFormat.Json ->
                                            { content = content
                                            , fileName = fileName
                                            , contentType = DataFormat.Json
                                            , fileUploadErrorOccurred = Nothing
                                            }

                                        DataFormat.Csv ->
                                            { content = content
                                            , fileName = fileName
                                            , contentType = DataFormat.Csv
                                            , fileUploadErrorOccurred = Nothing
                                            }

                                        DataFormat.Other ->
                                            { fileUploadEntry | fileUploadErrorOccurred = Just File.UnsupportedFileType }

                                File.ReadError readError ->
                                    { fileUploadEntry | fileUploadErrorOccurred = Just readError }
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

                ( ( AzureImportTab azureTab, id ), changeMsg ) ->
                    case changeMsg of
                        AzureConnectionStringChange connectionString ->
                            ( AzureImportTab { azureTab | connectionString = connectionString }, id )

                        AzureContainerChange container ->
                            ( AzureImportTab { azureTab | container = container }, id )

                        AzureBlobChange blob ->
                            ( AzureImportTab { azureTab | blob = blob }, id )

                        _ ->
                            ( AzureImportTab azureTab, id )

                ( ( DirectDataTab dataTab, id ), changeMsg ) ->
                    case changeMsg of
                        DataChange data ->
                            let
                                trimmed =
                                    data |> String.trim

                                value =
                                    if String.isEmpty trimmed then
                                        Nothing
                                    else
                                        Just trimmed

                                -- uses first non-whitespace character to see if it's JSON
                                inputType =
                                    if trimmed |> String.startsWith "{" then
                                        DataFormat.Json
                                    else if trimmed |> String.contains "," then
                                        DataFormat.Csv
                                    else
                                        DataFormat.Other
                            in
                            ( DirectDataTab { dataTab | content = data, contentType = inputType }, id )

                        _ ->
                            ( DirectDataTab dataTab, id )

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


uploadIsLoading : Model -> Bool
uploadIsLoading model =
    let
        importIsLoading =
            case model.importResponse of
                Remote.Loading ->
                    True

                Remote.Success importDetail ->
                    importDetail.status /= Status.Failed && importDetail.status /= Status.Cancelled

                _ ->
                    False
    in
    importIsLoading || Remote.isLoading model.uploadResponse


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
                    viewSetKey context model
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12 right" ]
                [ viewButtons (configWizard context True)
                    model
                    model.steps
                    (uploadIsLoading model)
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
                [ viewButtons (configWizard context False)
                    model
                    model.steps
                    (uploadIsLoading model)
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


finalStepButton : Bool -> Model -> Wizard.HtmlDetails Msg
finalStepButton isBottom model =
    let
        buttonContent =
            case model.tabs.current of
                ( FileUploadTab _, _ ) ->
                    case model.uploadResponse of
                        Remote.Loading ->
                            [ waiting isBottom model ]

                        _ ->
                            [ text "Create DataSet" ]

                ( DirectDataTab _, _ ) ->
                    case model.uploadResponse of
                        Remote.Loading ->
                            [ waiting isBottom model ]

                        _ ->
                            [ text "Create DataSet" ]

                _ ->
                    case model.importResponse of
                        Remote.Loading ->
                            [ waiting isBottom model ]

                        Remote.Success importDetail ->
                            if importDetail.status == Status.Failed || importDetail.status == Status.Cancelled then
                                [ i [ class "fa fa-upload mr5" ] [], text "Import" ]
                            else
                                [ waiting isBottom model ]

                        _ ->
                            [ i [ class "fa fa-upload mr5" ] [], text "Import" ]
    in
    Wizard.HtmlDetails [] buttonContent


viewSetKey : ContextModel -> Model -> Html Msg
viewSetKey context model =
    let
        errorDisplay =
            case model.tabs.current of
                ( FileUploadTab _, _ ) ->
                    viewRemoteError model.uploadResponse

                ( DirectDataTab _, _ ) ->
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
                [ viewButtons (configWizard context False) model model.steps (uploadIsLoading model) (model.errors == [])
                ]
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
                    [ explainer context.config "why_choose_key"
                    ]
                ]
            ]
        ]


viewEntryReview : Model -> List (Html Msg)
viewEntryReview model =
    let
        dataSetName =
            ( "DataSet Name", formatDisplayName model.name )

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

                ( AzureImportTab azureImport, _ ) ->
                    [ ( "Connection String", formatDisplayNameWithWidth 30 azureImport.connectionString )
                    , ( "Container", azureImport.container )
                    , ( "Blob", azureImport.blob )
                    ]

                ( DirectDataTab direct, _ ) ->
                    []
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
                    viewUploadTab context fileUploadEntry model

                ( UrlImportTab urlImportEntry, _ ) ->
                    viewImportUrlTab context.config urlImportEntry model

                ( S3ImportTab s3ImportEntry, _ ) ->
                    viewImportS3Tab context.config s3ImportEntry model

                ( AzureImportTab azureImportEntry, _ ) ->
                    viewImportAzureTab context.config azureImportEntry model

                ( DirectDataTab directDataEntry, _ ) ->
                    viewDirectDataTab context.config directDataEntry model
    in
    div [ class "tab-content" ]
        [ div [ class "tab-pane active" ]
            [ content ]
        ]


viewUploadTab : ContextModel -> FileUploadEntry -> Model -> Html Msg
viewUploadTab context tabModel model =
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
                , viewJust
                    (\errorType ->
                        case errorType of
                            File.FileTooLarge ->
                                div [ class "alert alert-danger" ] [ text ("The file you are attempting to upload is too large for your subscription level. Your account currently has a limit of " ++ (maxSize context.quotas |> dataSizeWithCustomKSize 1000) ++ " per dataset. Please select a smaller file, or reduce the size of this dataset and try again.") ]

                            File.UnsupportedFileType ->
                                div [ class "alert alert-danger" ] [ text "Only JSON or CSV file types are supported." ]

                            File.UnknownError ->
                                div [ class "alert alert-danger" ] [ text "An error occurred when uploading the file.  Please ensure it is a valid JSON or CSV file and less than 1 MB in size.  Larger files may be uploaded via one of the other import methods." ]
                    )
                    tabModel.fileUploadErrorOccurred
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "how_upload_csv"
                ]
            ]
        ]


viewDirectDataTab : Config -> DirectDataEntry -> Model -> Html Msg
viewDirectDataTab config tabModel model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-12" ]
                [ label [] [ text "Data" ]
                , textarea [ class "form-control", rows 20, cols 100, onInput <| \c -> TabMsg (DataChange c), value tabModel.content, onBlur InputBlur ] []
                , viewFieldError model.errors DataField
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ explainer config "how_paste_data"
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
                [ label [] [ text "Bucket Name", span [ class "text-danger" ] [ text "*" ] ]
                , input [ class "form-control", required True, onInput <| \c -> TabMsg (S3BucketChange c), value tabModel.bucket, onBlur InputBlur ] []
                , viewFieldError model.errors AwsBucketField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "File Path", span [ class "text-danger" ] [ text "*" ] ]
                , input [ class "form-control", required True, onInput <| \c -> TabMsg (S3PathChange c), value tabModel.path, onBlur InputBlur ] []
                , viewFieldError model.errors AwsPathField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "AWS Region" ]
                , viewRegions model tabModel.region
                , viewFieldError model.errors AwsRegionField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "Access Key Id" ]
                , input [ class "form-control", onInput <| \c -> TabMsg (S3AccessKeyIdChange c), value <| Maybe.withDefault "" tabModel.accessKeyId, onBlur InputBlur ] []
                , viewFieldError model.errors AwsAccessTokenIdField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "Secret Access Key" ]
                , input [ class "form-control", onInput <| \c -> TabMsg (S3SecretAccessKeyChange c), value <| Maybe.withDefault "" tabModel.secretAccessKey, onBlur InputBlur ] []
                , viewFieldError model.errors AwsSecretAccessKeyField
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ explainer config "how_upload_s3"
                ]
            ]
        ]


viewImportAzureTab : Config -> AzureImportEntry -> Model -> Html Msg
viewImportAzureTab config tabModel model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-8" ]
                [ label [] [ text "Connection String", span [ class "text-danger" ] [ text "*" ] ]
                , input [ class "form-control", required True, onInput <| \c -> TabMsg (AzureConnectionStringChange c), value tabModel.connectionString, onBlur InputBlur ] []
                , viewFieldError model.errors AzureConnectionStringField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "Container", span [ class "text-danger" ] [ text "*" ] ]
                , input [ class "form-control", required True, onInput <| \c -> TabMsg (AzureContainerChange c), value tabModel.container, onBlur InputBlur ] []
                , viewFieldError model.errors AzureContainerField
                ]
            , div [ class "form-group col-sm-8" ]
                [ label [] [ text "Blob", span [ class "text-danger" ] [ text "*" ] ]
                , input [ class "form-control", required True, onInput <| \c -> TabMsg (AzureBlobChange c), value tabModel.blob, onBlur InputBlur ] []
                , viewFieldError model.errors AzureBlobField
                ]
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ explainer config "how_upload_azure"
                ]
            ]
        ]


viewRegions : Model -> Maybe String -> Html Msg
viewRegions model selectedRegion =
    let
        isSelected val selected =
            case selected of
                Nothing ->
                    False

                Just s ->
                    val == s

        optionize ( val, name ) =
            option [ value val, selected <| isSelected val selectedRegion ]
                [ text name ]
    in
    select [ class "form-control", onInput <| \c -> TabMsg (S3RegionChange c) ]
        (List.map optionize model.awsRegions.regions)
