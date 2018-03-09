module Page.Home exposing (Model, Msg(..), init, update, view)

import AppRoutes
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString, toDataSetName)
import Data.Model exposing (ModelData, ModelList)
import Data.Response exposing (Quota, Quotas, Response)
import Data.Session exposing (SessionData, SessionList)
import Data.Subscription exposing (Subscription)
import Html exposing (..)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Page.DataSets as DataSets exposing (viewDataSetGridReadonly)
import Page.Helpers exposing (..)
import Page.Models as Models exposing (viewModelGridReadonly)
import Page.Sessions as Sessions exposing (viewSessionGridReadonly)
import RemoteData as Remote
import Request.DataSet
import Request.Model
import Request.Session
import Request.Subscription
import Table
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { dataSetList : Remote.WebData DataSetList
    , sessionList : Remote.WebData SessionList
    , modelList : Remote.WebData ModelList
    , subscriptionList : Remote.WebData (List Subscription)
    , keysShown : List String
    , quotas : Maybe Quotas
    , config : Config
    }


init : Config -> Maybe Quotas -> ( Model, Cmd Msg )
init config quotas =
    Model
        Remote.Loading
        Remote.Loading
        Remote.Loading
        Remote.Loading
        []
        quotas
        config
        => Cmd.batch
            [ Request.DataSet.get config 0 5
                |> Remote.sendRequest
                |> Cmd.map DataSetListResponse
            , Request.Session.get config 0 5
                |> Remote.sendRequest
                |> Cmd.map SessionListResponse
            , Request.Model.get config 0 5
                |> Remote.sendRequest
                |> Cmd.map ModelListResponse
            , Request.Subscription.list config
                |> Remote.sendRequest
                |> Cmd.map SubscriptionListResponse
            ]



-- UPDATE --


type Msg
    = None
    | DataSetListResponse (Remote.WebData DataSetList)
    | SessionListResponse (Remote.WebData SessionList)
    | ModelListResponse (Remote.WebData ModelList)
    | SubscriptionListResponse (Remote.WebData (List Subscription))
    | ShowApiKey String
    | QuotasUpdated (Maybe Quotas)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toggleKeyShown id =
            case List.member id model.keysShown of
                False ->
                    id :: model.keysShown

                True ->
                    List.filter (\a -> a /= id) model.keysShown
    in
    case msg of
        None ->
            ( model, Cmd.none )

        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SessionListResponse resp ->
            { model | sessionList = resp } => Cmd.none

        ModelListResponse resp ->
            { model | modelList = resp } => Cmd.none

        SubscriptionListResponse resp ->
            { model | subscriptionList = resp } => Cmd.none

        ShowApiKey id ->
            { model | keysShown = toggleKeyShown id } => Cmd.none

        QuotasUpdated quotas ->
            { model | quotas = quotas } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "API Dashboard" ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12 col-md-8 col-g-9 col-xl-9" ]
                [ viewRecentPanel "Dataset" (dataSetListView model) (AppRoutes.DataSets => Just AppRoutes.DataSetAdd)
                , viewRecentPanel "Session" (sessionListView model) (AppRoutes.Sessions => Nothing)
                , viewRecentPanel "Model" (modelListView model) (AppRoutes.Models => Nothing)
                ]
            , div [ class "col-sm-12 col-md-4 col-lg-3 col-xl-3" ]
                [ viewSidePanel (loadingOrView model.subscriptionList (viewSubscriptions model))
                ]
            , div [ class "col-sm-12 col-md-4 col-lg-3 col-xl-3" ]
                [ viewSidePanel (viewQuotas model.quotas)
                ]
            ]
        ]


viewQuotas : Maybe Quotas -> Html Msg
viewQuotas quotas =
    case quotas of
        Nothing ->
            div [] []

        Just quotas ->
            div []
                [ div [ class "row m0" ]
                    [ h4 [ class "mb15" ]
                        [ text "Usage Stats" ]
                    , viewQuota "DataSets" quotas.dataSets
                    , viewQuota "Sessions" quotas.sessions
                    , viewQuota "Predictions" quotas.predictions
                    ]
                ]


viewQuota : String -> Quota -> Html Msg
viewQuota name quota =
    let
        progressBarType : Int -> String
        progressBarType percentUsed =
            if percentUsed < 80 then
                "success"
            else if percentUsed < 95 then
                "warning"
            else
                "error"

        percentUsed : Quota -> Int
        percentUsed quota =
            (toFloat (Maybe.withDefault 0 quota.current) / toFloat (Maybe.withDefault 1 quota.allotted))
                |> (\a -> a * 100)
                |> truncate

        value val =
            Maybe.withDefault 0 val
    in
    div []
        [ p [] [ text name ]
        , div [ class "progress" ]
            [ div
                [ class ("progress-bar progress-bar-" ++ (progressBarType <| percentUsed quota))
                , attribute "role" "progressbar"
                , attribute "aria-valuenow" (value quota.current |> toString)
                , attribute "aria-valuemin" "0"
                , attribute "aria-valuemax" (value quota.allotted |> toString)
                , attribute "style" ("width: " ++ toString (percentUsed quota) ++ "%")
                ]
                [ text ((value quota.current |> toString) ++ "/" ++ (value quota.allotted |> toString)) ]
            ]
        , hr [] []
        ]


viewSidePanel : Html Msg -> Html Msg
viewSidePanel view =
    div [ class "panel" ]
        [ div [ class "panel-body p15" ]
            [ view
            ]
        ]


viewSubscriptions : Model -> List Subscription -> Html Msg
viewSubscriptions model subscriptions =
    div [ class "row m0" ]
        [ h4 [ class "mb15" ]
            [ strong
                []
                [ text "API Keys" ]
            ]
        , div
            []
            (subscriptions
                |> List.map (viewSubscription model)
            )
        ]


viewSubscription : Model -> Subscription -> Html Msg
viewSubscription model subscription =
    div []
        [ p [ class "mb0" ]
            [ strong [] [ text subscription.name ]
            , a [ class "obfuscate ml15" ] [ i [ class "fa fa-copy" ] [] ]
            , button
                [ class "btn btn-sm btn-link"
                , onClick (ShowApiKey subscription.id)
                ]
                [ i [ class "fa fa-eye mr5" ] []
                ]
            , p [ class "obfuscate" ]
                [ viewApiKey model subscription
                ]
            ]
        ]


viewApiKey : Model -> Subscription -> Html Msg
viewApiKey model subscription =
    case List.member subscription.id model.keysShown of
        False ->
            text "XXXXXXXXXXXXXXXXXXXXXXXXXXXX"

        True ->
            text subscription.key


modelListView : Model -> Html Msg
modelListView model =
    viewModelGridReadonly model.config.toolTips (Table.initialSort "createdDate") model.modelList |> Html.map (\_ -> None)


dataSetListView : Model -> Html Msg
dataSetListView model =
    viewDataSetGridReadonly model.config.toolTips (Table.initialSort "dataSetName") model.dataSetList |> Html.map (\_ -> None)


sessionListView : Model -> Html Msg
sessionListView model =
    viewSessionGridReadonly model.config.toolTips (Table.initialSort "name") model.sessionList |> Html.map (\_ -> None)


viewRecentPanel : String -> Html Msg -> ( AppRoutes.Route, Maybe AppRoutes.Route ) -> Html Msg
viewRecentPanel thing view ( linkRoute, addRoute ) =
    let
        addButton addRoute =
            case addRoute of
                Nothing ->
                    div [] []

                Just route ->
                    a [ AppRoutes.href route, class "btn btn-danger btn-sm" ]
                        [ i [ class "fa fa-plus" ] []
                        , text (" Add " ++ String.toLower thing)
                        ]
    in
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-6 p10" ]
                    [ h4 [] [ strong [] [ text ("Recent " ++ thing ++ "s") ] ]
                    ]
                , div [ class "col-sm-6 pt5 pr0 right" ]
                    [ a [ AppRoutes.href linkRoute, class "btn btn-primary btn-sm mr10" ] [ text ("View All " ++ thing ++ "s") ]
                    , addButton addRoute
                    ]
                ]
            , hr [ class "mt10" ] []
            , view
            ]
        ]
