module Page.SessionStart exposing (Model, Msg, init, subscriptions, update, view)

import AppRoutes exposing (Route)
import Data.Columns
import Data.Context exposing (ContextModel, contextToAuth)
import Data.DisplayDate exposing (toShortDateString, toShortDateTimeString)
import Data.Ziplist as Ziplist exposing (Ziplist)
import Date exposing (Date, Month(..))
import Date.Extra as Date
import DateTimePicker
import DateTimePicker.Config exposing (defaultDatePickerConfig, defaultDateTimePickerConfig)
import DateTimePicker.SharedStyles
import Dict exposing (Dict)
import Dom.Scroll as Scroll
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onCheck, onClick, onInput)
import List.Extra as List
import Maybe.Verify
import Nexosis.Api.Data
import Nexosis.Api.Sessions exposing (ForecastSessionRequest, ImpactSessionRequest, ModelSessionRequest, postForecast, postImpact, postModel)
import Nexosis.Types.Columns as Columns exposing (ColumnMetadata, ColumnStats)
import Nexosis.Types.DataSet exposing (DataSetData, DataSetName, DataSetStats, dataSetNameToString)
import Nexosis.Types.PredictionDomain as PredictionDomain exposing (PredictionDomain(..))
import Nexosis.Types.Session as Session exposing (ResultInterval(..), SessionData)
import Page.Helpers exposing (explainer, explainerFormat)
import RemoteData as Remote
import Select exposing (fromSelected)
import String.Extra as String
import String.Verify exposing (notBlank)
import Task
import Time.DateTime as DateTime exposing (DateTime, zero)
import Time.TimeZone as TimeZone
import Time.TimeZones exposing (fromName, utc)
import Time.ZonedDateTime as ZonedDateTime exposing (ZonedDateTime, fromDateTime)
import Tuple2 as Tuple
import Util exposing ((=>), dateToUtcDateTime, formatDisplayName, isJust, spinner, styledNumber, tryParseAndFormat, unwrapErrors)
import Verify exposing (Validator)
import View.Breadcrumb as Breadcrumb
import View.ColumnMetadataEditor as ColumnMetadataEditor
import View.Error exposing (viewFieldError, viewRemoteError)
import View.Extra exposing (viewJust)
import View.TimeZonePicker exposing (timeZoneList)
import View.Wizard as Wizard exposing (StepValidator, WizardConfig, WizardProgressConfig, viewButtons, viewProgress)


type alias Model =
    { steps : Ziplist Step
    , dataSetName : DataSetName
    , dataSetResponse : Remote.WebData DataSetData
    , columnEditorModel : ColumnMetadataEditor.Model
    , sessionColumnMetadata : List ColumnMetadata
    , sessionName : Maybe String
    , missingValuesText : Maybe String
    , missingValues : List String
    , selectedSessionType : Maybe PredictionDomain
    , sessionStartRequest : Remote.WebData SessionData
    , startDate : Maybe Date
    , endDate : Maybe Date
    , timeZone : TimeZone.TimeZone
    , startDatePickerState : DateTimePicker.State
    , endDatePickerState : DateTimePicker.State
    , resultInterval : ResultInterval
    , eventName : Maybe String
    , containsAnomalies : Bool
    , balance : Bool
    , errors : List FieldError
    , stats : Dict String (Remote.WebData ColumnStats)
    , target : Maybe String
    }


type Msg
    = NextStep
    | PrevStep
    | InputBlur
    | ChangeSessionName String
    | SelectSessionType PredictionDomain
    | DataSetDataResponse (Remote.WebData DataSetData)
    | ColumnMetadataEditorMsg ColumnMetadataEditor.Msg
    | StartTheSession SessionRequest
    | StartSessionResponse (Remote.WebData SessionData)
    | StartDateChanged DateTimePicker.State (Maybe Date)
    | EndDateChanged DateTimePicker.State (Maybe Date)
    | ChangeEventName String
    | IntervalChanged ResultInterval
    | TimeZoneSelected String
    | SelectContainsAnomalies Bool
    | SelectBalance Bool
    | SetWizardPage Step
    | NoOp
    | ChangeMissingValues String


type Step
    = NameSession
    | SelectDataSet
    | SessionType
    | StartEndDates
    | ContainsAnomalies
    | SetBalance
    | ColumnMetadata
    | StartSession


type SessionRequest
    = ForecastRequest ForecastSessionRequest
    | ImpactRequest ImpactSessionRequest
    | ModelRequest ModelSessionRequest



-- todo : change datasetname to a maybe


init : ContextModel -> DataSetName -> ( Model, Cmd Msg )
init context dataSetName =
    let
        steps =
            Ziplist.create [] NameSession [ SessionType, ColumnMetadata, StartSession ]

        loadDataSetRequest =
            Nexosis.Api.Data.getRetrieveDetail (contextToAuth context) dataSetName 0 1
                |> Remote.sendRequest
                |> Cmd.map DataSetDataResponse

        ( editorModel, initCmd ) =
            ColumnMetadataEditor.init dataSetName True
    in
    Model
        steps
        dataSetName
        Remote.Loading
        editorModel
        []
        Nothing
        Nothing
        []
        Nothing
        Remote.NotAsked
        Nothing
        Nothing
        (utc ())
        DateTimePicker.initialState
        DateTimePicker.initialState
        Session.Day
        Nothing
        True
        True
        []
        Dict.empty
        Nothing
        ! [ loadDataSetRequest
          , Cmd.map ColumnMetadataEditorMsg initCmd
          , DateTimePicker.initialCmd StartDateChanged DateTimePicker.initialState
          , DateTimePicker.initialCmd EndDateChanged DateTimePicker.initialState
          ]


type alias FieldError =
    ( Field, String )


type Field
    = EventNameField
    | StartDateField
    | SessionTypeField
    | MetadataField
    | TimeSeriesField


validateModel : Model -> Result (List FieldError) SessionRequest
validateModel model =
    case model.selectedSessionType of
        Just Forecast ->
            validateForecast model
                |> Result.map ForecastRequest

        Just Impact ->
            validateImpact model
                |> Result.map ImpactRequest

        Just _ ->
            validateModelRequest model
                |> Result.map ModelRequest

        _ ->
            Err [ SessionTypeField => "Session Type Selection is required" ]


verifyStartEndDates : Model -> Result (List FieldError) { endDate : ZonedDateTime, startDate : ZonedDateTime }
verifyStartEndDates { startDate, endDate, timeZone } =
    case ( startDate, endDate ) of
        ( Just startDate, Just endDate ) ->
            let
                start =
                    startDate |> dateToUtcDateTime timeZone

                end =
                    endDate |> dateToUtcDateTime timeZone
            in
            if DateTime.compare start end == LT then
                Ok { startDate = ZonedDateTime.fromDateTime timeZone start, endDate = ZonedDateTime.fromDateTime timeZone end }
            else
                Err [ StartDateField => "Start date must be before the end date." ]

        _ ->
            Err [ StartDateField => "A start date and an end date are required." ]


verifyEventName : Model -> Result (List FieldError) String
verifyEventName { eventName, selectedSessionType } =
    case selectedSessionType of
        Just Impact ->
            eventName
                |> Maybe.Verify.isJust (EventNameField => "An event name is required")
                |> Result.andThen (String.Verify.notBlank (EventNameField => "An event name is required"))

        _ ->
            Ok ""


keepBalance : Model -> Result (List FieldError) (Maybe Bool)
keepBalance { balance, selectedSessionType } =
    case selectedSessionType of
        Just Classification ->
            Ok (Just balance)

        _ ->
            Ok Nothing


keepContainsAnomalies : Model -> Result (List FieldError) (Maybe Bool)
keepContainsAnomalies { containsAnomalies, selectedSessionType } =
    case selectedSessionType of
        Just Anomalies ->
            Ok (Just containsAnomalies)

        _ ->
            Ok Nothing


verifySessionType : Model -> List FieldError
verifySessionType model =
    List.concatMap (\f -> f model)
        [ verifySessionTypeSelected >> unwrapErrors
        , verifyHasDate >> unwrapErrors
        ]


verifySessionTypeSelected : Model -> Result (List FieldError) PredictionDomain
verifySessionTypeSelected model =
    Maybe.Verify.isJust (SessionTypeField => "A Session type must be selected.") model.selectedSessionType


verifyHasDate : Model -> Result (List FieldError) String
verifyHasDate model =
    let
        metadata =
            dateColumnCandidate <| getMetaDataColumns model

        isTimeSeries =
            case model.selectedSessionType of
                Just Forecast ->
                    True

                Just Impact ->
                    True

                _ ->
                    False
    in
    if isTimeSeries && metadata.dataType /= Columns.Date then
        Err [ SessionTypeField => "The selected session type requires time-series data, but this dataset does not have a date field from which to provide a time-series forecast. Please select a dataset which has time-series information, or select a different type of session to build." ]
    else
        Ok ""


validateForecast : Validator FieldError Model ForecastSessionRequest
validateForecast =
    Verify.ok ForecastSessionRequest
        |> Verify.keep .sessionName
        |> Verify.keep .dataSetName
        |> Verify.keep .sessionColumnMetadata
        |> Verify.keep .missingValues
        |> Verify.custom verifyStartEndDates
        |> Verify.keep .resultInterval


validateImpact : Validator FieldError Model ImpactSessionRequest
validateImpact =
    Verify.ok ImpactSessionRequest
        |> Verify.keep .sessionName
        |> Verify.keep .dataSetName
        |> Verify.keep .sessionColumnMetadata
        |> Verify.keep .missingValues
        |> Verify.custom verifyStartEndDates
        |> Verify.custom verifyEventName
        |> Verify.keep .resultInterval


validateModelRequest : Validator FieldError Model ModelSessionRequest
validateModelRequest =
    Verify.ok ModelSessionRequest
        |> Verify.keep .sessionName
        |> Verify.keep .dataSetName
        |> Verify.keep .sessionColumnMetadata
        |> Verify.keep .missingValues
        |> Verify.custom verifySessionTypeSelected
        |> Verify.custom keepBalance
        |> Verify.custom keepContainsAnomalies


perStepValidations : List ( Step, Model -> List FieldError )
perStepValidations =
    [ ( StartEndDates, verifyStartEndStep )
    , ( SessionType, verifySessionType )
    , ( ColumnMetadata, verifyMetadataStep )
    ]


verifyStartEndStep : Model -> List FieldError
verifyStartEndStep model =
    List.concatMap (\f -> f model)
        [ verifyStartEndDates >> unwrapErrors
        , verifyEventName >> unwrapErrors
        ]


verifyMetadataStep : Model -> List FieldError
verifyMetadataStep model =
    List.concatMap (\f -> f model)
        [ verifyTimestampRole >> unwrapErrors
        , verifyTarget >> unwrapErrors
        ]


verifyTimestampRole : Model -> Result (List FieldError) String
verifyTimestampRole model =
    let
        shouldValidate =
            model.selectedSessionType == Just Forecast || model.selectedSessionType == Just Impact

        merged =
            Dict.values model.columnEditorModel.modifiedMetadata |> mergeMetadata (getMetaDataColumns model)

        dateCandidate =
            dateColumnCandidate merged

        hasDate =
            dateCandidate.dataType == Columns.Date

        hasTimestamp =
            dateCandidate.role == Columns.Timestamp

        errorMessage =
            if hasTimestamp then
                ""
            else if hasDate then
                "You are executing a forecast against a dataset that does not have a timestamp role. You must select a date column to use as the timestamp and set that role before starting the session."
            else
                "Dataset contains neither timestamp nor date column. We cannot run time-series algorithms without a date value of some kind."
    in
    if shouldValidate && String.length errorMessage > 0 then
        Err [ MetadataField => errorMessage ]
    else
        Ok ""


mergeMetadata : List Columns.ColumnMetadata -> List Columns.ColumnMetadata -> List Columns.ColumnMetadata
mergeMetadata left right =
    List.filter (\b -> List.member b.name (List.map (\a -> a.name) left)) right
        ++ List.filterNot (\b -> List.member b.name (List.map (\a -> a.name) right)) left


verifyTarget : Model -> Result (List FieldError) String
verifyTarget model =
    if model.columnEditorModel.showTarget && model.target == Nothing then
        Err [ MetadataField => "A target column must be selected either as a role in the metadata editor or in the target text box." ]
    else
        Ok ""


configWizard : WizardConfig Step FieldError Msg Model SessionRequest
configWizard =
    { nextMessage = NextStep
    , prevMessage = PrevStep
    , stepValidation = perStepValidations
    , finishedButton = \_ -> Wizard.HtmlDetails [] [ text "Start Session" ]
    , finishedValidation = validateModel
    , finishedMsg = StartTheSession
    , customLoading = Nothing
    }


configWizardSummary : WizardProgressConfig Step
configWizardSummary =
    { stepDescriptions =
        [ ( NameSession, "Session Name" )
        , ( SelectDataSet, "DataSet Name" )
        , ( SessionType, "Session Type" )
        , ( StartEndDates, "Start/End Dates" )
        , ( ContainsAnomalies, "Contains Anomalies" )
        , ( SetBalance, "Set Balance" )
        , ( ColumnMetadata, "Column Metadata" )
        , ( StartSession, "Start Session" )
        ]
    }


defaultRemainingSteps : List Step
defaultRemainingSteps =
    [ ColumnMetadata, StartSession ]


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case ( model.steps.current, msg ) of
        ( NameSession, ChangeSessionName sessionName ) ->
            let
                name =
                    if String.isEmpty <| String.trim sessionName then
                        Nothing
                    else
                        Just sessionName
            in
            { model | sessionName = name } => Cmd.none

        ( NameSession, ChangeMissingValues missingValuesText ) ->
            let
                ( values, text ) =
                    if String.isEmpty missingValuesText then
                        [] => Nothing
                    else
                        String.split "," missingValuesText => Just missingValuesText
            in
            { model | missingValuesText = text, missingValues = values } => Cmd.none

        ( SessionType, SelectSessionType sessionType ) ->
            let
                columnModel =
                    model.columnEditorModel

                maxDate =
                    extractTimestampMax model

                startingDate =
                    if sessionType == Forecast && model.startDate == Nothing then
                        maxDate
                    else if sessionType == Impact then
                        Nothing
                    else
                        model.startDate

                ( steps, showTarget ) =
                    if sessionType == Forecast || sessionType == Impact then
                        Ziplist.create model.steps.previous model.steps.current (StartEndDates :: defaultRemainingSteps)
                            => True
                    else if sessionType == Anomalies then
                        Ziplist.create model.steps.previous model.steps.current (ContainsAnomalies :: defaultRemainingSteps)
                            => False
                    else if sessionType == Classification then
                        Ziplist.create model.steps.previous model.steps.current (SetBalance :: defaultRemainingSteps)
                            => True
                    else
                        Ziplist.create model.steps.previous model.steps.current defaultRemainingSteps
                            => True

                columnEditorModel =
                    { columnModel | showTarget = showTarget }
            in
            { model | selectedSessionType = Just sessionType, steps = steps, columnEditorModel = columnEditorModel, errors = [], startDate = startingDate } => Cmd.none

        ( StartSession, StartTheSession request ) ->
            let
                sessionRequest =
                    case request of
                        ForecastRequest forecastRequest ->
                            postForecast (contextToAuth context) forecastRequest
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        ImpactRequest impactRequest ->
                            postImpact (contextToAuth context) impactRequest
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        ModelRequest modelRequest ->
                            postModel (contextToAuth context) modelRequest
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse
            in
            { model | sessionStartRequest = Remote.Loading }
                => sessionRequest

        ( _, StartDateChanged state value ) ->
            { model
                | startDate = value
                , startDatePickerState = state
            }
                |> recheckErrors
                => Cmd.none

        ( _, EndDateChanged state value ) ->
            { model
                | endDate = value
                , endDatePickerState = state
            }
                |> recheckErrors
                => Cmd.none

        ( StartEndDates, TimeZoneSelected tz ) ->
            let
                timeZone =
                    fromName tz |> Maybe.withDefault (utc ())
            in
            { model | timeZone = timeZone } => Cmd.none

        ( StartEndDates, IntervalChanged interval ) ->
            { model | resultInterval = interval }
                => Cmd.none

        ( StartEndDates, ChangeEventName eventName ) ->
            { model | eventName = Just eventName }
                => Cmd.none

        ( ContainsAnomalies, SelectContainsAnomalies isSelected ) ->
            { model | containsAnomalies = isSelected } => Cmd.none

        ( SetBalance, SelectBalance isSelected ) ->
            { model | balance = isSelected } => Cmd.none

        ( _, StartSessionResponse resp ) ->
            let
                ( newModel, cmd ) =
                    case resp of
                        Remote.Success session ->
                            { model | sessionStartRequest = resp }
                                => AppRoutes.newUrl (AppRoutes.SessionDetail session.sessionId)

                        _ ->
                            { model | sessionStartRequest = resp }
                                => Cmd.none
            in
            newModel => cmd

        ( _, NextStep ) ->
            let
                errors =
                    validateStep model
            in
            if errors == [] then
                { model | steps = Ziplist.advance model.steps }
                    => (Task.attempt (always NoOp) <| Scroll.toTop "html")
            else
                { model | errors = errors } => Cmd.none

        ( _, PrevStep ) ->
            { model | steps = Ziplist.rewind model.steps, errors = [] }
                => (Task.attempt (always NoOp) <| Scroll.toTop "html")

        ( _, InputBlur ) ->
            recheckErrors model => Cmd.none

        ( _, DataSetDataResponse resp ) ->
            let
                ( subModel, cmd ) =
                    ColumnMetadataEditor.updateDataSetResponse context model.columnEditorModel resp

                target =
                    case resp of
                        Remote.Success dataSetData ->
                            getTargetColumn dataSetData.columns

                        _ ->
                            model.target
            in
            { model | dataSetResponse = resp, columnEditorModel = subModel, target = target }
                => Cmd.map ColumnMetadataEditorMsg cmd

        ( _, ColumnMetadataEditorMsg subMsg ) ->
            let
                ( ( newModel, cmd ), updateCmd ) =
                    ColumnMetadataEditor.update subMsg
                        model.columnEditorModel
                        context
                        (\_ -> Task.succeed () |> Remote.asCmd)

                modifiedMetadata =
                    case updateCmd of
                        ColumnMetadataEditor.Updated modifiedMetadata ->
                            modifiedMetadata

                        _ ->
                            model.sessionColumnMetadata

                newTarget =
                    mergeMetadata (getMetaDataColumns model) modifiedMetadata
                        |> getTargetColumn
            in
            { model
                | columnEditorModel = newModel
                , sessionColumnMetadata = modifiedMetadata
                , stats = newModel.statsResponse
                , target = newTarget
            }
                |> recheckErrors
                => Cmd.map ColumnMetadataEditorMsg cmd

        ( _, SetWizardPage step ) ->
            let
                newSteps =
                    Ziplist.find (\s -> s == step) model.steps
                        |> Maybe.withDefault model.steps
            in
            { model | steps = newSteps } => Cmd.none

        _ ->
            model => Cmd.none


recheckErrors : Model -> Model
recheckErrors model =
    if model.errors /= [] then
        { model | errors = validateStep model }
    else
        model


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


getTargetColumn : List Columns.ColumnMetadata -> Maybe String
getTargetColumn metadata =
    metadata
        |> List.find (\c -> c.role == Columns.Target)
        |> Maybe.map .name


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.steps.current == ColumnMetadata then
        ColumnMetadataEditor.subscriptions model.columnEditorModel
            |> Sub.map ColumnMetadataEditorMsg
    else
        Sub.none


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ id "page-header", class "row" ]
            [ Breadcrumb.list
            , div [ class "col-sm-6" ] [ h2 [] [ text "Start a session" ] ]
            ]
        , div [ class "row mb20" ]
            ([ viewProgress configWizardSummary model.steps |> Html.map never ]
                ++ wizardPage context model
                ++ [ div [ class "col-sm-12" ]
                        [ div [ class "col-sm-12 well well-sm right" ]
                            [ viewButtons configWizard model model.steps (Remote.isLoading model.sessionStartRequest) (model.errors == []) ]
                        ]
                   ]
            )
        ]


wizardPage : ContextModel -> Model -> List (Html Msg)
wizardPage context model =
    let
        title =
            wizardTitle context
    in
    case model.steps.current of
        NameSession ->
            [ title model "Name your session"
            , viewNameSession context model
            ]

        SelectDataSet ->
            [ title model "Select a dataset"
            , viewSelectDataSet model
            ]

        SessionType ->
            [ title model "Choose a session type"
            , viewSessionType context model
            ]

        StartEndDates ->
            if model.selectedSessionType == Just Impact then
                [ title model "Event Details"
                , viewImpactStartEndDates context model
                ]
            else
                [ title model "Select start and end dates", viewStartEndDates context model ]

        ContainsAnomalies ->
            [ title model "Does your dataset contain anomalies?", viewContainsAnomalies context model ]

        SetBalance ->
            [ title model "Set balance", viewSetBalance context model ]

        ColumnMetadata ->
            [ title model "Edit your column metadata", viewColumnMetadata context model ]

        StartSession ->
            [ title model "Please confirm your session setup", viewStartSession model ]


wizardTitle : ContextModel -> Model -> String -> Html Msg
wizardTitle context model title =
    div [ class "col-sm-12 session-step" ]
        [ div [ class "col-sm-6 pl0" ] [ h3 [ class "mt0" ] [ text title ] ]
        , div [ class "col-sm-6 right" ]
            [ viewButtons configWizard model model.steps (Remote.isLoading model.sessionStartRequest) (model.errors == []) ]
        ]


viewNameSession : ContextModel -> Model -> Html Msg
viewNameSession context model =
    div [ class "col-sm-12" ]
        [ div [ class "form-group col-sm-4" ]
            [ label [] [ text "Session name" ]
            , input [ class "form-control", value <| Maybe.withDefault "" model.sessionName, onInput ChangeSessionName, placeholder "(Optional)" ] []
            ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "how_name_session" ]
            ]
        , div [ class "col-sm-12" ]
            [ hr [] []
            , h3 [ class "mt0" ] [ text "Are there any values that should be treated as missing?" ]
            , div [ class "col-sm-4" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Missing Values" ]
                    , input [ type_ "text", class "form-control", placeholder "(Optional)", value <| Maybe.withDefault "" model.missingValuesText, onInput ChangeMissingValues ] []
                    ]
                ]
            , div [ class "col-sm-6 col-sm-offset-2" ]
                [ div [ class "alert alert-info" ]
                    [ explainer context.config "how_set_missing" ]
                ]
            ]
        ]


viewSelectDataSet : Model -> Html Msg
viewSelectDataSet model =
    div [ class "col-sm-12" ]
        [ div [ class "form-group col-sm-4" ]
            [ div [ class "input-group" ]
                [ span [ class "input-group-addon" ] [ i [ class "fa fa-search" ] [] ]
                , input [ class "form-control", value "" ] []
                ]
            ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ h5 [] [ text "Choosing a DataSet" ]
                , p [] [ text "Search for the DataSet you want to use to test and train your model." ]
                ]
            ]
        ]


sessionTypeToString : PredictionDomain -> String
sessionTypeToString sessionType =
    case sessionType of
        Classification ->
            "Classification"

        Regression ->
            "Regression"

        Forecast ->
            "Forecast"

        Impact ->
            "Impact Analysis"

        Anomalies ->
            "Anomaly Detection"


viewSessionType : ContextModel -> Model -> Html Msg
viewSessionType context model =
    div [ class "col-sm-12" ]
        [ div [ class "row" ] [ div [ class "col-sm-6" ] [ viewFieldError model.errors SessionTypeField ] ]
        , div [ class "form-group" ]
            [ sessionTypePanel
                "https://nexosis.com/assets/img/features/classification.png"
                (explainer context.config "session_classification")
                model.selectedSessionType
                Classification
            , sessionTypePanel
                "https://nexosis.com/assets/img/features/regression.png"
                (explainer context.config "session_regression")
                model.selectedSessionType
                Regression
            , sessionTypePanel
                "https://nexosis.com/assets/img/features/forecasting.png"
                (explainer context.config "session_forecasting")
                model.selectedSessionType
                Forecast
            , sessionTypePanel
                "https://nexosis.com/assets/img/features/impact-analysis.png"
                (explainer context.config "session_impact")
                model.selectedSessionType
                Impact
            , sessionTypePanel
                "https://nexosis.com/assets/img/features/anomaly-detection.png"
                (explainer context.config "session_anomaly")
                model.selectedSessionType
                Anomalies
            ]
        , div [ class "row" ] [ div [ class "col-sm-6" ] [ viewFieldError model.errors SessionTypeField ] ]
        ]


sessionTypePanel : String -> Html Msg -> Maybe PredictionDomain -> PredictionDomain -> Html Msg
sessionTypePanel imageUrl bodyHtml currentSelection sessionType =
    let
        isSelected =
            currentSelection == Just sessionType

        buttonContent =
            if isSelected then
                [ i [ class "fa fa-check-circle mr5" ] [], text "Selected" ]
            else
                [ i [ class "fa fa-circle-o mr5" ] [], text "Select" ]
    in
    div [ class "col-sm-4", onClick (SelectSessionType sessionType) ]
        [ div [ classList [ ( "panel ml-select", True ), ( "selected", isSelected ) ] ]
            [ div [ class "panel-heading center" ]
                [ img [ src imageUrl ] []
                , h3 [ class "panel-title center" ] [ text <| sessionTypeToString sessionType ]
                ]
            , div [ class "panel-body" ]
                (bodyHtml
                    :: [ hr [ class "mt10 mb10" ] []
                       , div [ class "center" ]
                            [ button
                                [ classList
                                    [ ( "btn", True )
                                    , ( "other", not isSelected )
                                    , ( "btn-primary", isSelected )
                                    ]
                                , onClick (SelectSessionType sessionType)
                                ]
                                buttonContent
                            ]
                       ]
                )
            ]
        ]


viewStartEndDates : ContextModel -> Model -> Html Msg
viewStartEndDates context model =
    div [ class "col-sm-12" ]
        [ viewStartEndDateExplainer context model
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "Result Interval" ]
            , fromSelected [ Session.Hour, Session.Day, Session.Week, Session.Month, Session.Year ] IntervalChanged model.resultInterval
            ]
        , viewTzList model
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "Start date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , viewDatePicker model StartDateChanged [] model.startDatePickerState model.startDate
                ]
            , viewFieldError model.errors StartDateField
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "End date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , viewDatePicker model EndDateChanged [] model.endDatePickerState model.endDate
                ]
            ]
        ]


viewStartEndDateExplainer : ContextModel -> Model -> Html Msg
viewStartEndDateExplainer context model =
    let
        ( min, max ) =
            getMinMaxValueFromCandidate model
                |> Tuple.mapBoth
                    (\date ->
                        if String.isBlank date then
                            "(Unknown)"
                        else
                            tryParseAndFormat date
                    )
    in
    div [ class "help col-sm-6 pull-right" ]
        [ div [ class "alert alert-info" ]
            [ explainerFormat context.config "session_forecast_start_end" [ min, max ] ]
        ]


viewImpactStartEndDates : ContextModel -> Model -> Html Msg
viewImpactStartEndDates context model =
    let
        ( min, max ) =
            getMinMaxValueFromCandidate model
                |> Tuple.mapBoth
                    (\date ->
                        if String.isBlank date then
                            "(Unknown)"
                        else
                            tryParseAndFormat date
                    )
    in
    div [ class "col-sm-12" ]
        [ div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ explainerFormat context.config "session_impact_start_end" [ min, max ] ]
            ]
        , div [ class "form-group col-sm-3" ]
            [ label [] [ text "Event name" ]
            , input [ class "form-control", onInput ChangeEventName, onBlur InputBlur, value <| (model.eventName |> Maybe.withDefault "") ] []
            , viewFieldError model.errors EventNameField
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "Result Interval" ]
            , fromSelected [ Session.Hour, Session.Day, Session.Week, Session.Month, Session.Year ] IntervalChanged model.resultInterval
            ]
        , viewTzList model
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "Start date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , viewDatePicker model StartDateChanged [] model.startDatePickerState model.startDate
                ]
            , viewFieldError model.errors StartDateField
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "End date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , viewDatePicker model EndDateChanged [] model.endDatePickerState model.endDate
                ]
            ]
        ]


viewTzList : Model -> Html Msg
viewTzList model =
    case model.resultInterval of
        Session.Hour ->
            div [ class "form-group col-sm-3 clearfix-left" ]
                [ label [] [ text "Timezone" ]
                , TimeZone.name model.timeZone |> timeZoneList TimeZoneSelected
                ]

        _ ->
            div [] []


viewDatePicker : Model -> (DateTimePicker.State -> Maybe Date -> Msg) -> List (Attribute Msg) -> DateTimePicker.State -> Maybe Date -> Html Msg
viewDatePicker model msg =
    if model.resultInterval == Session.Hour then
        DateTimePicker.dateTimePickerWithConfig (dateTimePickerConfig msg)
    else
        DateTimePicker.datePickerWithConfig (defaultDatePickerConfig msg)


dateTimePickerConfig : (DateTimePicker.State -> Maybe Date -> Msg) -> DateTimePicker.Config.Config (DateTimePicker.Config.CssConfig (DateTimePicker.Config.DatePickerConfig DateTimePicker.Config.TimePickerConfig) Msg DateTimePicker.SharedStyles.CssClasses) Msg
dateTimePickerConfig msg =
    let
        defaultDateTimeConfig =
            defaultDateTimePickerConfig msg
    in
    { defaultDateTimeConfig
        | timePickerType = DateTimePicker.Config.Digital
    }


viewContainsAnomalies : ContextModel -> Model -> Html Msg
viewContainsAnomalies context model =
    div [ class "col-sm-12" ]
        [ div [ class "form-group col-sm-6" ]
            [ label [ class "radio", for "anomalies-yes" ]
                [ input [ id "anomalies-yes", name "anomalies", checked <| model.containsAnomalies, type_ "radio", onClick (SelectContainsAnomalies True) ] []
                , text "Yes, my DataSet contains anomalies."
                ]
            , label [ class "radio", for "anomalies-no" ]
                [ input [ id "anomalies-no", name "anomalies", checked <| not model.containsAnomalies, type_ "radio", onClick (SelectContainsAnomalies False) ] []
                , text "No, my DataSet does not contain anomalies."
                ]
            ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "session_anomaly_details"
                ]
            ]
        ]


viewSetBalance : ContextModel -> Model -> Html Msg
viewSetBalance context model =
    div [ class "col-sm-12" ]
        [ div [ class "form-group col-sm-6" ]
            [ label [ class "radio", for "balance-yes" ]
                [ input [ id "balance-yes", name "balance", checked <| model.balance, type_ "radio", onClick (SelectBalance True) ] []
                , text "Yes, balance my test set."
                ]
            , label [ class "radio", for "balance-no" ]
                [ input [ id "balance-no", name "balance", checked <| not model.balance, type_ "radio", onClick (SelectBalance False) ] []
                , text "No, don't balance my test set."
                ]
            ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "session_classification_balance"
                ]
            ]
        ]


viewColumnMetadata : ContextModel -> Model -> Html Msg
viewColumnMetadata context model =
    div [ class "col-sm-12" ]
        [ div [ class "help col-sm-6 pull-right" ]
            [ viewFieldError model.errors MetadataField
            , div [ class "alert alert-info" ]
                [ explainer context.config "session_column_metadata"
                ]
            ]
        , div [ class "form-group col-sm-6" ]
            [ ColumnMetadataEditor.viewTargetAndKeyColumns context model.columnEditorModel
                |> Html.map ColumnMetadataEditorMsg
            ]
        , hr [] []
        , ColumnMetadataEditor.view context model.columnEditorModel |> Html.map ColumnMetadataEditorMsg
        ]


type EditType
    = Locked
    | EditStep Step


boolToString : Bool -> String
boolToString bool =
    if bool then
        "Yes"
    else
        "No"


viewStartSession : Model -> Html Msg
viewStartSession model =
    let
        maybeContainsAnomalies =
            if model.selectedSessionType == Just Anomalies then
                Just <| boolToString model.containsAnomalies
            else
                Nothing

        maybeSetBalance =
            if model.selectedSessionType == Just Classification then
                Just <| boolToString model.balance
            else
                Nothing

        maybeStartEndDates =
            Maybe.map2
                (\start end ->
                    if model.resultInterval == Hour then
                        toShortDateTimeString start ++ " - " ++ toShortDateTimeString end
                    else
                        toShortDateString start ++ " - " ++ toShortDateString end
                )
                (model.startDate |> Maybe.map (\d -> d |> dateToUtcDateTime model.timeZone |> ZonedDateTime.fromDateTime model.timeZone))
                (model.endDate |> Maybe.map (\d -> d |> dateToUtcDateTime model.timeZone |> ZonedDateTime.fromDateTime model.timeZone))

        maybeResultInterval =
            Maybe.map (\_ -> toString model.resultInterval) model.startDate

        properties =
            [ ( "Session Name", model.sessionName, EditStep NameSession )
            , ( "DataSet Name", Just <| formatDisplayName <| dataSetNameToString model.dataSetName, Locked )
            , ( "Session Type", Maybe.map sessionTypeToString model.selectedSessionType, EditStep SessionType )
            , ( "Contains Anomalies", maybeContainsAnomalies, EditStep ContainsAnomalies )
            , ( "Set Balance", maybeSetBalance, EditStep SetBalance )
            , ( "Event Name", model.eventName, EditStep StartEndDates )
            , ( "Result Interval", maybeResultInterval, EditStep StartEndDates )
            , ( "Start/End Dates", maybeStartEndDates, EditStep StartEndDates )
            , ( "Target", model.target, EditStep ColumnMetadata )
            , ( "Column Metadata", Just "Done", EditStep ColumnMetadata )
            ]
    in
    div [ id "review", class "col-sm-9" ]
        (List.map reviewItem properties
            ++ [ hr [] []
               , div [ class "row" ] [ viewRemoteError model.sessionStartRequest ]
               ]
        )


reviewItem : ( String, Maybe String, EditType ) -> Html Msg
reviewItem ( name, maybeValue, editType ) =
    viewJust
        (\value ->
            div [ class "form-group col-sm-4" ]
                [ p [] [ text name ]
                , h6 [] [ text value, editIcon editType ]
                ]
        )
        maybeValue


editIcon : EditType -> Html Msg
editIcon editType =
    case editType of
        Locked ->
            i [ class "fa fa-lock" ] []

        EditStep step ->
            i [ class "fa fa-edit", onClick (SetWizardPage step) ] []


extractTimestampMax : Model -> Maybe Date
extractTimestampMax model =
    let
        ( _, dateString ) =
            getMinMaxValueFromCandidate model

        date =
            dateString |> String.replaceSlice "" 19 100 |> Date.fromIsoString
    in
    case date of
        Result.Ok date ->
            Just date

        Result.Err err ->
            Nothing


getMetaDataColumns : Model -> List Columns.ColumnMetadata
getMetaDataColumns model =
    case model.columnEditorModel.columnMetadata of
        Remote.Success cm ->
            Dict.values cm.metadata

        _ ->
            []


dateColumnCandidate : List Columns.ColumnMetadata -> Columns.ColumnMetadata
dateColumnCandidate list =
    let
        column =
            List.filter (\a -> a.dataType == Columns.Date) list
                |> List.append (List.filter (\a -> a.role == Columns.Timestamp) list)
                |> List.head
    in
    Maybe.withDefault Data.Columns.defaultColumnMetadata column


getMinMaxValueFromCandidate : Model -> ( String, String )
getMinMaxValueFromCandidate model =
    let
        metadata =
            dateColumnCandidate <| getMetaDataColumns model

        stat =
            Dict.get metadata.name model.stats
                |> Maybe.map (\s -> Remote.toMaybe s)
                |> Maybe.andThen (\s -> s)
    in
    case stat of
        Just s ->
            ( s.min, s.max )

        Nothing ->
            ( "", "" )
