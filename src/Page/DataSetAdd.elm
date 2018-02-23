module Page.DataSetAdd exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes exposing (Route)
import Data.Config exposing (Config)
import Data.DataSet
import Data.Ziplist as Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode exposing (succeed)
import Navigation
import Ports exposing (fileContentRead, uploadFileSelected)
import RemoteData as Remote
import Request.DataSet exposing (put)
import Request.Log as Log
import Util exposing ((=>), spinner)
import Validate exposing (Validator, ifBlank, validate)
import View.Error exposing (viewRemoteError)
import View.Extra exposing (viewIf)
import View.Wizard exposing (WizardConfig, viewButtons)


type alias Model =
    { config : Config
    , steps : Ziplist Step
    , canAdvance : Bool
    , name : String
    , key : String
    , activeTab : Tab

    -- todo - probably want to end up splitting this into types with records.
    -- direct uploads vs. imports will be handled quite differently, will need very different request payloads.
    , fileContent : String
    , fileName : String
    , fileUploadType : FileUploadType
    , fileUploadErrorOccurred : Bool
    , uploadResponse : Remote.WebData ()
    }


type Step
    = ChooseUploadType
    | SetKey


type Tab
    = Upload
    | Import
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


init : Config -> ( Model, Cmd Msg )
init config =
    let
        steps =
            Ziplist.create ChooseUploadType [ SetKey ]
    in
    Model config steps False "" "" Upload "" "" Other False Remote.NotAsked
        => Cmd.none



-- Validation


firstStepValidator : Validator String Model
firstStepValidator =
    Validate.all
        [ ifBlank .name "DataSet name required."
        , ifBlank .fileContent "Choose a file to upload."
        ]


setModelValid : Model -> Model
setModelValid model =
    { model | canAdvance = validate firstStepValidator model |> List.isEmpty }



-- Update


type Msg
    = ChangeName String
    | ChangeKey String
    | NextStep
    | PrevStep
    | ChangeTab Tab
    | FileSelected
    | FileContentRead Json.Decode.Value
    | CreateDataSet
    | UploadDataSetResponse (Remote.WebData ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.steps.current, msg ) of
        ( ChooseUploadType, ChangeName name ) ->
            { model | name = name }
                |> setModelValid
                => Cmd.none

        ( ChooseUploadType, ChangeTab tab ) ->
            { model | activeTab = tab } => Cmd.none

        ( ChooseUploadType, FileSelected ) ->
            model => Ports.uploadFileSelected "upload-dataset"

        ( ChooseUploadType, FileContentRead readResult ) ->
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
                                        | fileContent = content
                                        , fileName = fileName
                                        , fileUploadType = Json
                                        , fileUploadErrorOccurred = False
                                    }
                                        |> setModelValid

                                Csv ->
                                    { model
                                        | fileContent = content
                                        , fileName = fileName
                                        , fileUploadType = Csv
                                        , fileUploadErrorOccurred = False
                                    }
                                        |> setModelValid

                                Other ->
                                    { model | fileUploadErrorOccurred = True }

                        ReadError ->
                            { model | fileUploadErrorOccurred = True }
            in
            m => Cmd.none

        ( SetKey, ChangeKey key ) ->
            { model | key = key } => Cmd.none

        ( SetKey, CreateDataSet ) ->
            let
                putRequest =
                    put model.config model.name model.fileContent (fileUploadTypeToContentType model.fileUploadType)
                        |> Remote.sendRequest
                        |> Cmd.map UploadDataSetResponse
            in
            { model | uploadResponse = Remote.Loading } => putRequest

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

        ( _, NextStep ) ->
            { model | steps = Ziplist.advance model.steps } => Cmd.none

        ( _, PrevStep ) ->
            { model | steps = Ziplist.rewind model.steps } => Cmd.none

        _ ->
            model => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileContentRead


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
                [ viewButtons configWizard model.canAdvance model.steps
                ]
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
        createButtonContent =
            case model.uploadResponse of
                Remote.Loading ->
                    spinner

                _ ->
                    text "Create DataSet"
    in
    div [ class "col-sm-12" ]
        [ div [ id "review" ]
            [ div [ class "form-group col-sm-3" ]
                [ p [] [ text "DataSet Name" ]
                , h6 [] [ text model.name ]
                ]
            , div [ class "form-group col-sm-3" ]
                [ p [] [ text "Filename" ]
                , h6 [] [ text model.fileName ]
                ]
            ]
        , hr [] []
        , h3 [ class "mt0" ] [ text "Do you want to specify a key?" ]
        , div [ class "col-sm-4" ]
            [ div [ class "form-group" ]
                [ label [] [ text "Key" ]
                , input [ class "form-control", placeholder "(Optional)", value model.key ] []
                ]
            , div [ class "form-group" ]
                [ button [ class "btn", onClick CreateDataSet ] [ createButtonContent ]
                ]
            , viewRemoteError model.uploadResponse
            ]
        , div [ class "col-sm-6" ]
            [ div [ class "alert alert-info" ]
                [ h5 [] [ text "Why choosing a key is important" ]
                , p [] [ text "Tell them why it matters." ]
                ]
            ]
        ]


viewTabControl : Model -> Html Msg
viewTabControl model =
    let
        tabHeaders =
            [ li [ classList [ ( "active", model.activeTab == Upload ) ] ] [ a [ onClick (ChangeTab Upload) ] [ text "Upload" ] ]
            , li [ classList [ ( "active", model.activeTab == Import ) ] ] [ a [ onClick (ChangeTab Import) ] [ text "Import from URL" ] ]
            , li [ classList [ ( "active", model.activeTab == PasteIn ) ] ] [ a [ onClick (ChangeTab PasteIn) ] [ text "Paste DataSet" ] ]
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

                PasteIn ->
                    viewPasteInTab model
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
                [ h5 [] [ text "How to upload a CSV/JSON" ]
                , p [] [ text "Instructions go here." ]
                ]
            ]
        ]


viewImportUrlTab : Model -> Html Msg
viewImportUrlTab model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            []
        , div [ class "col-sm-6" ]
            []
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
