module Page.SessionStart exposing (Model, Msg, init, update, view)

import AppRoutes exposing (Route)
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSetName)
import Data.Ziplist as Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import RemoteData as Remote
import Util exposing ((=>))
import View.Wizard exposing (WizardConfig, WizardProgressConfig, viewButtons, viewProgress)


type alias Model =
    { config : Config
    , steps : Ziplist Step
    , canAdvance : Bool
    , dataSetName : DataSetName
    , selectedSessionType : Maybe SessionType
    , sessionName : String
    }


type Msg
    = NextStep
    | PrevStep
    | ChangeSessionName String
    | SelectSessionType SessionType


type Step
    = NameSession
    | SelectDataSet
    | SessionType
    | ColumnMetadata
    | StartSession


type SessionType
    = Classification
    | Regression
    | Forecasting
    | ImpactAnalysis
    | AnomalyDetection



-- todo : change datasetname to a maybe


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    let
        steps =
            Ziplist.create NameSession [ SessionType, ColumnMetadata, StartSession ]
    in
    Model config steps True dataSetName Nothing ""
        => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.steps.current, msg ) of
        ( NameSession, ChangeSessionName sessionName ) ->
            { model | sessionName = sessionName } => Cmd.none

        ( SessionType, SelectSessionType sessionType ) ->
            { model | selectedSessionType = Just sessionType, canAdvance = True } => Cmd.none

        ( _, NextStep ) ->
            { model | steps = Ziplist.advance model.steps } => Cmd.none

        ( _, PrevStep ) ->
            { model | steps = Ziplist.rewind model.steps } => Cmd.none

        _ ->
            model => Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-sm-6" ] [ h2 [ class "mt10" ] [ text "Start a session" ] ]
            ]
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

            ColumnMetadata ->
                div [] []

            StartSession ->
                div [] []
        , viewButtons configWizard model.canAdvance model.steps
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
                        Forecasting
                   , sessionTypePanel
                        "https://nexosis.com/assets/img/features/impact-analysis.png"
                        "Impact Analysis"
                        (p []
                            [ text "Impact analysis, a type of forecasting, uncovers the effect of past events on your data. "
                            , strong [] [ text "If you want to know what if, impact analysis has your answers." ]
                            ]
                        )
                        model.selectedSessionType
                        ImpactAnalysis
                   , sessionTypePanel
                        "https://nexosis.com/assets/img/features/anomaly-detection.png"
                        "Anomaly Detection"
                        (p []
                            [ text "Anomaly detection discovers the unusual and outliers in your data. "
                            , strong [] [ text "If you want to know what's weird, anomaly detection has your back." ]
                            ]
                        )
                        model.selectedSessionType
                        AnomalyDetection
                   ]
            )
        ]


sessionTypePanel : String -> String -> Html Msg -> Maybe SessionType -> SessionType -> Html Msg
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
    div [ class "col-sm-4" ]
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
        , ( ColumnMetadata, "Column Metadata" )
        , ( StartSession, "Start Session" )
        ]
    }
