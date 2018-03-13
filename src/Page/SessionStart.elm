module Page.SessionStart exposing (Model, Msg, init, update, view)

import AppRoutes exposing (Route)
import Data.Columns as Columns
import Data.Config exposing (Config)
import Data.Context exposing (ContextModel)
import Data.DataSet exposing (DataSetData, DataSetName, dataSetNameToString)
import Data.PredictionDomain as PredictionDomain exposing (PredictionDomain(..))
import Data.Session as Session exposing (ResultInterval, SessionData)
import Data.Ziplist as Ziplist exposing (Ziplist)
import Date exposing (Date)
import DateTimePicker
import DateTimePicker.Config exposing (defaultDateTimePickerConfig)
import DateTimePicker.SharedStyles
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onCheck, onClick, onInput)
import List.Extra as List
import Maybe.Verify
import Page.Helpers exposing (explainer)
import RemoteData as Remote
import Request.DataSet
import Request.Session exposing (ForecastSessionRequest, ImpactSessionRequest, ModelSessionRequest, postForecast, postImpact, postModel)
import Select exposing (fromSelected)
import String.Verify exposing (notBlank)
import Time.DateTime as DateTime exposing (DateTime)
import Util exposing ((=>), isJust, spinner, unwrapErrors)
import Verify exposing (Validator)
import View.ColumnMetadataEditor as ColumnMetadataEditor
import View.Error exposing (viewFieldError, viewRemoteError)
import View.Wizard as Wizard exposing (StepValidator, WizardConfig, WizardProgressConfig, viewButtons, viewProgress)


type alias Model =
    { steps : Ziplist Step
    , dataSetName : DataSetName
    , dataSetResponse : Remote.WebData DataSetData
    , columnEditorModel : ColumnMetadataEditor.Model
    , sessionColumnMetadata : List Columns.ColumnMetadata
    , sessionName : String
    , selectedSessionType : Maybe PredictionDomain
    , sessionStartRequest : Remote.WebData SessionData
    , startDate : Maybe DateTime
    , endDate : Maybe DateTime
    , startDatePickerState : DateTimePicker.State
    , endDatePickerState : DateTimePicker.State
    , resultInterval : ResultInterval
    , eventName : Maybe String
    , containsAnomalies : Bool
    , balance : Bool
    , errors : List FieldError
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
    | SelectContainsAnomalies Bool
    | SelectBalance Bool


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


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    let
        steps =
            Ziplist.create [] NameSession [ SessionType, ColumnMetadata, StartSession ]

        loadDataSetRequest =
            Request.DataSet.getRetrieveDetail config dataSetName
                |> Remote.sendRequest
                |> Cmd.map DataSetDataResponse

        ( editorModel, initCmd ) =
            ColumnMetadataEditor.init config dataSetName True
    in
    Model
        steps
        dataSetName
        Remote.Loading
        editorModel
        []
        ""
        Nothing
        Remote.NotAsked
        Nothing
        Nothing
        DateTimePicker.initialState
        DateTimePicker.initialState
        Session.Day
        Nothing
        True
        True
        []
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


verifyStartEndDates : Model -> Result (List FieldError) { endDate : DateTime, startDate : DateTime }
verifyStartEndDates { startDate, endDate } =
    case ( startDate, endDate ) of
        ( Just startDate, Just endDate ) ->
            if DateTime.compare startDate endDate == LT then
                Ok { startDate = startDate, endDate = endDate }
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


verifySessionType : Model -> Result (List FieldError) PredictionDomain
verifySessionType model =
    Maybe.Verify.isJust (SessionTypeField => "A Session type must be selected.") model.selectedSessionType


validateForecast : Validator FieldError Model ForecastSessionRequest
validateForecast =
    Verify.ok ForecastSessionRequest
        |> Verify.keep .sessionName
        |> Verify.keep .dataSetName
        |> Verify.keep .sessionColumnMetadata
        |> Verify.custom verifyStartEndDates
        |> Verify.keep .resultInterval


validateImpact : Validator FieldError Model ImpactSessionRequest
validateImpact =
    Verify.ok ImpactSessionRequest
        |> Verify.keep .sessionName
        |> Verify.keep .dataSetName
        |> Verify.keep .sessionColumnMetadata
        |> Verify.custom verifyStartEndDates
        |> Verify.custom verifyEventName
        |> Verify.keep .resultInterval


validateModelRequest : Validator FieldError Model ModelSessionRequest
validateModelRequest =
    Verify.ok ModelSessionRequest
        |> Verify.keep .sessionName
        |> Verify.keep .dataSetName
        |> Verify.keep .sessionColumnMetadata
        |> Verify.custom verifySessionType
        |> Verify.custom keepBalance
        |> Verify.custom keepContainsAnomalies


perStepValidations : List ( Step, Model -> List FieldError )
perStepValidations =
    [ ( StartEndDates, verifyStartEndStep )
    , ( SessionType, verifySessionType >> unwrapErrors )
    ]


verifyStartEndStep : Model -> List FieldError
verifyStartEndStep model =
    List.concatMap (\f -> f model)
        [ verifyStartEndDates >> unwrapErrors
        , verifyEventName >> unwrapErrors
        ]


configWizard : WizardConfig Step FieldError Msg Model SessionRequest
configWizard =
    { nextMessage = NextStep
    , prevMessage = PrevStep
    , stepValidation = perStepValidations
    , finishedButton = \_ -> Wizard.HtmlDetails [] [ text "Start Session" ]
    , finishedValidation = validateModel
    , finishedMsg = StartTheSession
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
            { model | sessionName = sessionName } => Cmd.none

        ( SessionType, SelectSessionType sessionType ) ->
            let
                columnModel =
                    model.columnEditorModel

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
            { model | selectedSessionType = Just sessionType, steps = steps, columnEditorModel = columnEditorModel, errors = [] } => Cmd.none

        ( StartSession, StartTheSession request ) ->
            let
                sessionRequest =
                    case request of
                        ForecastRequest forecastRequest ->
                            postForecast context.config forecastRequest
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        ImpactRequest impactRequest ->
                            postImpact context.config impactRequest
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        ModelRequest modelRequest ->
                            postModel context.config modelRequest
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse
            in
            { model | sessionStartRequest = Remote.Loading }
                => sessionRequest

        ( _, StartDateChanged state value ) ->
            { model
                | startDate = value |> Maybe.map (Date.toTime >> DateTime.fromTimestamp)
                , startDatePickerState = state
            }
                |> recheckErrors
                => Cmd.none

        ( _, EndDateChanged state value ) ->
            { model
                | endDate = value |> Maybe.map (Date.toTime >> DateTime.fromTimestamp)
                , endDatePickerState = state
            }
                |> recheckErrors
                => Cmd.none

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
                { model | steps = Ziplist.advance model.steps } => Cmd.none
            else
                { model | errors = errors } => Cmd.none

        ( _, PrevStep ) ->
            { model | steps = Ziplist.rewind model.steps } => Cmd.none

        ( _, InputBlur ) ->
            recheckErrors model => Cmd.none

        ( _, DataSetDataResponse resp ) ->
            let
                ( subModel, cmd ) =
                    ColumnMetadataEditor.updateDataSetResponse model.columnEditorModel resp
            in
            { model | dataSetResponse = resp, columnEditorModel = subModel }
                => Cmd.map ColumnMetadataEditorMsg cmd

        ( _, ColumnMetadataEditorMsg subMsg ) ->
            let
                ( ( newModel, cmd ), updateCmd ) =
                    ColumnMetadataEditor.update subMsg model.columnEditorModel

                modifiedMetadata =
                    case updateCmd of
                        ColumnMetadataEditor.Updated modifiedMetadata ->
                            modifiedMetadata

                        _ ->
                            model.sessionColumnMetadata
            in
            { model
                | columnEditorModel = newModel
                , sessionColumnMetadata = modifiedMetadata
            }
                => Cmd.map ColumnMetadataEditorMsg cmd

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


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ class "row" ]
            [ div [ class "col-sm-6" ] [ h2 [ class "mt10" ] [ text "Start a session" ] ] ]
        , hr [] []
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
    case model.steps.current of
        NameSession ->
            [ wizardTitle model "Name your session"
            , viewNameSession context model
            ]

        SelectDataSet ->
            [ wizardTitle model "Select a dataset"
            , viewSelectDataSet model
            ]

        SessionType ->
            [ wizardTitle model "Choose a session type"
            , viewSessionType context model
            ]

        StartEndDates ->
            if model.selectedSessionType == Just Impact then
                [ wizardTitle model "Event Details"
                , viewImpactStartEndDates context model
                ]
            else
                [ wizardTitle model "Select start and end dates", viewStartEndDates context model ]

        ContainsAnomalies ->
            [ wizardTitle model "Does your dataset contain anomalies?", viewContainsAnomalies context model ]

        SetBalance ->
            [ wizardTitle model "Set balance", viewSetBalance context model ]

        ColumnMetadata ->
            [ wizardTitle model "Edit your column metadata", viewColumnMetadata context model ]

        StartSession ->
            [ wizardTitle model "Please confirm your session setup", viewStartSession model ]


wizardTitle : Model -> String -> Html Msg
wizardTitle model title =
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
            , input [ class "form-control", value model.sessionName, onInput ChangeSessionName ] []
            ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "how_name_session"
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


viewSessionType : ContextModel -> Model -> Html Msg
viewSessionType context model =
    div [ class "col-sm-12" ]
        [ div [ class "form-group" ]
            [ sessionTypePanel
                "https://nexosis.com/assets/img/features/classification.png"
                "Classification"
                (explainer context.config "session_classification")
                model.selectedSessionType
                Classification
            , sessionTypePanel
                "https://nexosis.com/assets/img/features/regression.png"
                "Regression"
                (explainer context.config "session_regression")
                model.selectedSessionType
                Regression
            , sessionTypePanel
                "https://nexosis.com/assets/img/features/forecasting.png"
                "Forecasting"
                (explainer context.config "session_forecasting")
                model.selectedSessionType
                Forecast
            , sessionTypePanel
                "https://nexosis.com/assets/img/features/impact-analysis.png"
                "Impact Analysis"
                (explainer context.config "session_impact")
                model.selectedSessionType
                Impact
            , sessionTypePanel
                "https://nexosis.com/assets/img/features/anomaly-detection.png"
                "Anomaly Detection"
                (explainer context.config "session_anomaly")
                model.selectedSessionType
                Anomalies
            ]
        , div [ class "row" ] [ div [ class "col-sm-6" ] [ viewFieldError model.errors SessionTypeField ] ]
        ]


sessionTypePanel : String -> String -> Html Msg -> Maybe PredictionDomain -> PredictionDomain -> Html Msg
sessionTypePanel imageUrl title bodyHtml currentSelection selectCmd =
    let
        isSelected =
            currentSelection == Just selectCmd

        buttonContent =
            if isSelected then
                [ i [ class "fa fa-check-circle mr5" ] [], text "Selected" ]
            else
                [ i [ class "fa fa-circle-o mr5" ] [], text "Select" ]
    in
    div [ class "col-sm-4", onClick (SelectSessionType selectCmd) ]
        [ div [ classList [ ( "panel ml-select", True ), ( "selected", isSelected ) ] ]
            [ div [ class "panel-heading center" ]
                [ img [ src imageUrl ] []
                , h3 [ class "panel-title center" ] [ text title ]
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
                                , onClick (SelectSessionType selectCmd)
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
        [ div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "session_forecast_start_end" ]
            ]
        , div [ class "form-group col-sm-3" ]
            [ label [] [ text "Start date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , DateTimePicker.dateTimePickerWithConfig (datePickerConfig StartDateChanged) [] model.startDatePickerState (model.startDate |> toDate)
                ]
            , viewFieldError model.errors StartDateField
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "End date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , DateTimePicker.dateTimePickerWithConfig (datePickerConfig EndDateChanged) [] model.endDatePickerState (model.endDate |> toDate)
                ]
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "Result Interval" ]
            , fromSelected [ Session.Hour, Session.Day, Session.Week, Session.Month, Session.Year ] IntervalChanged model.resultInterval
            ]
        ]


viewImpactStartEndDates : ContextModel -> Model -> Html Msg
viewImpactStartEndDates context model =
    div [ class "col-sm-12" ]
        [ div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "session_impact_start_end" ]
            ]
        , div [ class "form-group col-sm-3" ]
            [ label [] [ text "Event name" ]
            , input [ class "form-control", onInput ChangeEventName, onBlur InputBlur, value <| (model.eventName |> Maybe.withDefault "") ] []
            , viewFieldError model.errors EventNameField
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "Start date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , DateTimePicker.dateTimePickerWithConfig (datePickerConfig StartDateChanged) [] model.startDatePickerState (model.startDate |> toDate)
                ]
            , viewFieldError model.errors StartDateField
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "End date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , DateTimePicker.dateTimePickerWithConfig (datePickerConfig EndDateChanged) [] model.endDatePickerState (model.endDate |> toDate)
                ]
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "Result Interval" ]
            , fromSelected [ Session.Hour, Session.Day, Session.Week, Session.Month, Session.Year ] IntervalChanged model.resultInterval
            ]
        ]


toDate : Maybe DateTime -> Maybe Date
toDate dateTime =
    case dateTime of
        Just dt ->
            dt
                |> DateTime.toISO8601
                |> Date.fromString
                |> Result.toMaybe

        Nothing ->
            Nothing


datePickerConfig : (DateTimePicker.State -> Maybe Date -> Msg) -> DateTimePicker.Config.Config (DateTimePicker.Config.CssConfig (DateTimePicker.Config.DatePickerConfig DateTimePicker.Config.TimePickerConfig) Msg DateTimePicker.SharedStyles.CssClasses) Msg
datePickerConfig msg =
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
            [ div [ class "alert alert-info" ]
                [ explainer context.config "session_column_metadata"
                ]
            ]
        , div [ class "form-group col-sm-6" ]
            [ ColumnMetadataEditor.viewTargetAndKeyColumns model.columnEditorModel
                |> Html.map ColumnMetadataEditorMsg
            ]
        , hr [] []
        , ColumnMetadataEditor.view model.columnEditorModel |> Html.map ColumnMetadataEditorMsg
        ]


viewStartSession : Model -> Html Msg
viewStartSession model =
    let
        -- todo : more work here
        properties =
            [ ( "Session Name", model.sessionName )
            , ( "DataSet Name", dataSetNameToString model.dataSetName )
            ]
    in
    div [ id "review", class "col-sm-12" ]
        (List.map reviewItem properties
            ++ [ hr [] []
               , div [ class "row" ] [ viewRemoteError model.sessionStartRequest ]
               ]
        )


reviewItem : ( String, String ) -> Html Msg
reviewItem ( name, value ) =
    div [ class "form-group col-sm-3" ]
        [ p [] [ text name ]
        , h6 [] [ text value, i [ class "fa fa-edit" ] [] ]
        ]
