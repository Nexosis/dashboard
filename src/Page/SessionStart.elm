module Page.SessionStart exposing (Model, Msg, init, update, view)

import AppRoutes exposing (Route)
import Data.Columns as Columns
import Data.Config exposing (Config)
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
import Html.Events exposing (onCheck, onClick, onInput)
import RemoteData as Remote
import Request.DataSet
import Request.Session exposing (postForecast, postImpact, postModel)
import Select exposing (fromSelected)
import Time.DateTime as DateTime exposing (DateTime)
import Util exposing ((=>), isJust, spinner)
import View.ColumnMetadataEditor as ColumnMetadataEditor
import View.Error exposing (viewRemoteError)
import View.Wizard exposing (WizardConfig, WizardProgressConfig, viewButtons, viewProgress)


type alias Model =
    { config : Config
    , steps : Ziplist Step
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
    }


type Msg
    = NextStep
    | PrevStep
    | ChangeSessionName String
    | SelectSessionType PredictionDomain
    | DataSetDataResponse (Remote.WebData DataSetData)
    | ColumnMetadataEditorMsg ColumnMetadataEditor.Msg
    | StartTheSession
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
            ColumnMetadataEditor.init config dataSetName
    in
    Model config
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
        ! [ loadDataSetRequest
          , Cmd.map ColumnMetadataEditorMsg initCmd
          , DateTimePicker.initialCmd StartDateChanged DateTimePicker.initialState
          , DateTimePicker.initialCmd EndDateChanged DateTimePicker.initialState
          ]


isValid : Model -> Bool
isValid model =
    case model.steps.current of
        SessionType ->
            isJust model.selectedSessionType

        StartEndDates ->
            let
                datesValid =
                    Maybe.map2 (\start end -> DateTime.compare start end == LT) model.startDate model.endDate
                        |> (==) (Just True)
            in
            if model.selectedSessionType == Just Impact then
                datesValid
                    && (case model.eventName of
                            Just e ->
                                not (String.isEmpty e)

                            Nothing ->
                                False
                       )
            else
                datesValid

        _ ->
            True


defaultRemainingSteps : List Step
defaultRemainingSteps =
    [ ColumnMetadata, StartSession ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.steps.current, msg ) of
        ( NameSession, ChangeSessionName sessionName ) ->
            { model | sessionName = sessionName } => Cmd.none

        ( SessionType, SelectSessionType sessionType ) ->
            let
                steps =
                    if sessionType == Forecast || sessionType == Impact then
                        Ziplist.create model.steps.previous model.steps.current (StartEndDates :: defaultRemainingSteps)
                    else if sessionType == Anomalies then
                        Ziplist.create model.steps.previous model.steps.current (ContainsAnomalies :: defaultRemainingSteps)
                    else if sessionType == Classification then
                        Ziplist.create model.steps.previous model.steps.current (SetBalance :: defaultRemainingSteps)
                    else
                        Ziplist.create model.steps.previous model.steps.current defaultRemainingSteps
            in
            { model | selectedSessionType = Just sessionType, steps = steps } => Cmd.none

        ( StartSession, StartTheSession ) ->
            let
                sessionRequest =
                    case model.selectedSessionType of
                        Just Regression ->
                            let
                                modelRequestRec =
                                    { name = model.sessionName
                                    , dataSourceName = model.dataSetName
                                    , targetColumn = Just ""
                                    , predictionDomain = PredictionDomain.Regression
                                    , columns = model.sessionColumnMetadata
                                    , balance = Nothing
                                    , containsAnomalies = Nothing
                                    }
                            in
                            postModel model.config modelRequestRec
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        Just Anomalies ->
                            let
                                modelRequest =
                                    { name = model.sessionName
                                    , dataSourceName = model.dataSetName
                                    , targetColumn = Nothing
                                    , predictionDomain = PredictionDomain.Anomalies
                                    , columns = model.sessionColumnMetadata
                                    , balance = Nothing
                                    , containsAnomalies = Just model.containsAnomalies
                                    }
                            in
                            postModel model.config modelRequest
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        Just Classification ->
                            let
                                modelRequest =
                                    { name = model.sessionName
                                    , dataSourceName = model.dataSetName
                                    , targetColumn = Just ""
                                    , predictionDomain = PredictionDomain.Classification
                                    , columns = model.sessionColumnMetadata
                                    , balance = Just model.balance
                                    , containsAnomalies = Nothing
                                    }
                            in
                            postModel model.config modelRequest
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        Just Forecast ->
                            let
                                forecastReq =
                                    { name = model.sessionName
                                    , dataSourceName = model.dataSetName
                                    , targetColumn = ""
                                    , startDate = model.startDate |> Maybe.withDefault (DateTime.dateTime DateTime.zero)
                                    , endDate = model.endDate |> Maybe.withDefault (DateTime.dateTime DateTime.zero)
                                    , columns = model.sessionColumnMetadata
                                    , resultInterval = model.resultInterval
                                    }
                            in
                            postForecast model.config forecastReq
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        Just Impact ->
                            let
                                impactReq =
                                    { name = model.sessionName
                                    , dataSourceName = model.dataSetName
                                    , targetColumn = ""
                                    , startDate = model.startDate |> Maybe.withDefault (DateTime.dateTime DateTime.zero)
                                    , endDate = model.endDate |> Maybe.withDefault (DateTime.dateTime DateTime.zero)
                                    , columns = model.sessionColumnMetadata
                                    , eventName = model.eventName |> Maybe.withDefault ""
                                    , resultInterval = model.resultInterval
                                    }
                            in
                            postImpact model.config impactReq
                                |> Remote.sendRequest
                                |> Cmd.map StartSessionResponse

                        Nothing ->
                            Cmd.none
            in
            { model | sessionStartRequest = Remote.Loading }
                => sessionRequest

        ( _, StartDateChanged state value ) ->
            { model | startDate = value |> Maybe.map (Date.toTime >> DateTime.fromTimestamp), startDatePickerState = state } => Cmd.none

        ( _, EndDateChanged state value ) ->
            { model | endDate = value |> Maybe.map (Date.toTime >> DateTime.fromTimestamp), endDatePickerState = state } => Cmd.none

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
            { model | steps = Ziplist.advance model.steps } => Cmd.none

        ( _, PrevStep ) ->
            { model | steps = Ziplist.rewind model.steps } => Cmd.none

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


view : Model -> Html Msg
view model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-sm-6" ] [ h2 [ class "mt10" ] [ text "Start a session" ] ] ]
        , hr [] []
        , div [ class "row mb20" ]
            [ viewProgress configWizardSummary model.steps |> Html.map never ]
        , case model.steps.current of
            NameSession ->
                viewNameSession model

            SelectDataSet ->
                viewSelectDataSet model

            SessionType ->
                viewSessionType model

            StartEndDates ->
                if model.selectedSessionType == Just Impact then
                    viewImpactStartEndDates model
                else
                    viewStartEndDates model

            ContainsAnomalies ->
                viewContainsAnomalies model

            SetBalance ->
                viewSetBalance model

            ColumnMetadata ->
                viewColumnMetadata model

            StartSession ->
                viewStartSession model
        , viewButtons configWizard (isValid model) model.steps
        ]


viewNameSession : Model -> Html Msg
viewNameSession model =
    div [ class "col-sm-12" ]
        [ div [ class "form-group col-sm-4" ]
            [ h3 [ class "mt0" ] [ text "Session name" ]
            , input [ class "form-control", value model.sessionName, onInput ChangeSessionName ] []
            ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ h5 [] [ text "How to name your session" ]
                , p [] [ text "Name your session something descriptive yet memorable so that you can easily recall the purpose of each session." ]
                ]
            ]
        ]


viewSelectDataSet : Model -> Html Msg
viewSelectDataSet model =
    div [ class "col-sm-12" ]
        [ div [ class "form-group col-sm-4" ]
            [ h3 [ class "mt0" ] [ text "Select a DataSet" ]
            , div [ class "input-group" ]
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


viewSessionType : Model -> Html Msg
viewSessionType model =
    div [ class "col-sm-12" ]
        [ div [ class "form-group col-sm-12" ]
            (h3 [ class "mt0" ] [ text "Choose a session type" ]
                :: [ sessionTypePanel
                        "https://nexosis.com/assets/img/features/classification.png"
                        "Classification"
                        (p []
                            [ text "Classification allows you to organize your data in labeled, logical, and consumable buckets. "
                            , strong [] [ text "If you want to know if something is or is not a thing, classification is for you." ]
                            ]
                        )
                        model.selectedSessionType
                        Classification
                   , sessionTypePanel
                        "https://nexosis.com/assets/img/features/regression.png"
                        "Regression"
                        (p []
                            [ text "Regression uncovers relationships in your data to estimate the unknown, missing, or unmeasured. "
                            , strong [] [ text "If you want to know the unknown, regression is a good choice." ]
                            ]
                        )
                        model.selectedSessionType
                        Regression
                   , sessionTypePanel
                        "https://nexosis.com/assets/img/features/forecasting.png"
                        "Forecasting"
                        (p []
                            [ text "Forecasting finds patterns in your time series data to predict what's next. "
                            , strong [] [ text "If you want to know the unknown, regression is a good choice." ]
                            ]
                        )
                        model.selectedSessionType
                        Forecast
                   , sessionTypePanel
                        "https://nexosis.com/assets/img/features/impact-analysis.png"
                        "Impact Analysis"
                        (p []
                            [ text "Impact analysis, a type of forecasting, uncovers the effect of past events on your data. "
                            , strong [] [ text "If you want to know what if, impact analysis has your answers." ]
                            ]
                        )
                        model.selectedSessionType
                        Impact
                   , sessionTypePanel
                        "https://nexosis.com/assets/img/features/anomaly-detection.png"
                        "Anomaly Detection"
                        (p []
                            [ text "Anomaly detection discovers the unusual and outliers in your data. "
                            , strong [] [ text "If you want to know what's weird, anomaly detection has your back." ]
                            ]
                        )
                        model.selectedSessionType
                        Anomalies
                   ]
            )
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
                                    , ( "secondary", isSelected )
                                    ]
                                , onClick (SelectSessionType selectCmd)
                                ]
                                buttonContent
                            ]
                       ]
                )
            ]
        ]


viewStartEndDates : Model -> Html Msg
viewStartEndDates model =
    div [ class "col-sm-12" ]
        [ h3 [ class "mt0" ] [ text "Select start and end dates" ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ h5 [] [ text "Forecasting start and end dates" ]
                , p [] [ text "Need info" ]
                , p [] [ a [] [ text "Read more in our documentation." ] ]
                ]
            ]
        , div [ class "form-group col-sm-3" ]
            [ label [] [ text "Start date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , DateTimePicker.dateTimePickerWithConfig (datePickerConfig StartDateChanged) [] model.startDatePickerState (model.startDate |> toDate)
                ]
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


viewImpactStartEndDates : Model -> Html Msg
viewImpactStartEndDates model =
    div [ class "col-sm-12" ]
        [ h3 [ class "mt0" ] [ text "Event Details" ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ h5 [] [ text "Event Info" ]
                , p [] [ text "Need info" ]
                , p [] [ a [] [ text "Read more in our documentation." ] ]
                ]
            ]
        , div [ class "form-group col-sm-3" ]
            [ label [] [ text "Event name" ]
            , input [ class "form-control", onInput ChangeEventName, value <| (model.eventName |> Maybe.withDefault "") ] []
            ]
        , div [ class "form-group col-sm-3 clearfix-left" ]
            [ label [] [ text "Start date" ]
            , div [ class "input-group" ]
                [ span [ class "input-group-addon" ]
                    [ i [ class "fa fa-calendar" ] [] ]
                , DateTimePicker.dateTimePickerWithConfig (datePickerConfig StartDateChanged) [] model.startDatePickerState (model.startDate |> toDate)
                ]
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


viewContainsAnomalies : Model -> Html Msg
viewContainsAnomalies model =
    div [ class "col-sm-12" ]
        [ h3 [ class "mt0" ] [ text "Does your DataSet contain anomalies?" ]
        , div [ class "form-group col-sm-6" ]
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
                [ h5 [] [ text "Why does it matter whether my data has anomalies or not?" ]
                , p [] [ text "Determining whether your dataset has anomalies will determine how the model is trained. If you don't have anomalies, the end model will treat anything unusual to the training set as anomaly. If it does, it will use the outliers to determine what an anomaly is. ", strong [] [ text "Your dataset has anomalies by default." ] ]
                , p [] [ a [] [ text "Learn more." ] ]
                ]
            ]
        ]


viewSetBalance : Model -> Html Msg
viewSetBalance model =
    div [ class "col-sm-12" ]
        [ h3 [ class "mt0" ] [ text "Set Balance" ]
        , div [ class "form-group col-sm-6" ]
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
                [ h5 [] [ text "Why should I balance my test set?" ]
                , p [] [ text "Balancing your data ensures that your test set has data from every label. ", strong [] [ text "Your test set is balanced by default." ] ]
                , p [] [ a [] [ text "Learn more." ] ]
                , hr [] []
                , div [ class "row well m15 p15" ]
                    [ div [ class "col-sm-6" ]
                        [ h6 [ class "center" ]
                            [ text "Not balanced" ]
                        , table [ class "table table-bordered", attribute "style" "background-color: #fff;" ]
                            [ tbody []
                                [ tr []
                                    [ th [ class "left" ]
                                        [ text "Negative" ]
                                    , td [ class "success" ]
                                        [ text "710" ]
                                    , td [ class "warning" ]
                                        [ text "63" ]
                                    , td [ class "warning" ]
                                        [ text "27" ]
                                    ]
                                , tr []
                                    [ th [ class "left" ]
                                        [ text "Neutral" ]
                                    , td [ class "danger" ]
                                        [ text "128" ]
                                    , td [ class "success" ]
                                        [ text "151" ]
                                    , td [ class "warning" ]
                                        [ text "29" ]
                                    ]
                                , tr []
                                    [ th [ class "left" ]
                                        [ text "Positive" ]
                                    , td [ class "warning" ]
                                        [ text "46" ]
                                    , td [ class "warning" ]
                                        [ text "31" ]
                                    , td [ class "success" ]
                                        [ text "166" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "col-sm-6" ]
                        [ h6 [ class "center" ]
                            [ text "Balanced" ]
                        , table [ class "table table-bordered", attribute "style" "background-color: #fff;" ]
                            [ tbody []
                                [ tr []
                                    [ th [ class "left" ]
                                        [ text "Negative" ]
                                    , td [ class "success" ]
                                        [ text "695" ]
                                    , td [ class "warning" ]
                                        [ text "72" ]
                                    , td [ class "warning" ]
                                        [ text "32" ]
                                    ]
                                , tr []
                                    [ th [ class "left" ]
                                        [ text "Neutral" ]
                                    , td [ class "danger" ]
                                        [ text "107" ]
                                    , td [ class "success" ]
                                        [ text "169" ]
                                    , td [ class "warning" ]
                                        [ text "30" ]
                                    ]
                                , tr []
                                    [ th [ class "left" ]
                                        [ text "Positive" ]
                                    , td [ class "warning" ]
                                        [ text "39" ]
                                    , td [ class "warning" ]
                                        [ text "29" ]
                                    , td [ class "success" ]
                                        [ text "176" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewColumnMetadata : Model -> Html Msg
viewColumnMetadata model =
    div [ class "col-sm-12" ]
        [ h3 [ class "mt0" ] [ text "Edit your column metadata" ]
        , div [ class "help col-sm-6 pull-right" ]
            [ div [ class "alert alert-info" ]
                [ h5 [] [ text "Working with column metadata" ]
                , p [] [ text " Column metadata determines how the data in each column will be handled." ]
                , p [] [ a [] [ text "Read more in our documentation." ] ]
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

        startButton =
            case model.sessionStartRequest of
                Remote.Loading ->
                    button [ class "btn" ] [ spinner ]

                _ ->
                    button [ class "btn", onClick StartTheSession ]
                        [ text "Start Session"
                        , i [ class "fa fa-chevron-right ml5" ] []
                        ]
    in
    div [ id "review", class "col-sm-12" ]
        ([ h3 [ class "mt0" ] [ text "Please confirm your session setup" ] ]
            ++ List.map reviewItem properties
            ++ [ hr [] []
               , div [ class "row" ] [ div [ class "form-group col-sm-12" ] [ startButton ] ]
               , div [ class "row" ] [ viewRemoteError model.sessionStartRequest ]
               ]
        )


reviewItem : ( String, String ) -> Html Msg
reviewItem ( name, value ) =
    div [ class "form-group col-sm-3" ]
        [ p [] [ text name ]
        , h6 [] [ text value, i [ class "fa fa-edit" ] [] ]
        ]


configWizard : WizardConfig Msg
configWizard =
    { nextMessage = NextStep
    , prevMessage = PrevStep
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
