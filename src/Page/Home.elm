module Page.Home exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString, toDataSetName)
import Data.Model exposing (ModelData, ModelList)
import Data.Session exposing (SessionData, SessionList)
import Data.Subscription exposing (Subscription)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra exposing (find)
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
    , subscriptionKeys : List (Remote.WebData Subscription)
    , config : Config
    }


init : Config -> ( Model, Cmd Msg )
init config =
    Model
        Remote.Loading
        Remote.Loading
        Remote.Loading
        Remote.Loading
        []
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
    | ApiKeyRetrieved (Remote.WebData Subscription)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
            ( model
            , Request.Subscription.getKey model.config id
                |> Remote.sendRequest
                |> Cmd.map ApiKeyRetrieved
            )

        ApiKeyRetrieved resp ->
            { model | subscriptionKeys = resp :: model.subscriptionKeys } => Cmd.none



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
            ]
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
    --please forgive me Zach
    let
        retrievedKeys : List Subscription
        retrievedKeys =
            List.filterMap maybeGetKey model.subscriptionKeys

        maybeGetKey : Remote.WebData Subscription -> Maybe Subscription
        maybeGetKey response =
            case response of
                Remote.Success sub ->
                    Just sub

                _ ->
                    Nothing

        getRetrievedOrUnretrieved : List Subscription -> Subscription -> Subscription
        getRetrievedOrUnretrieved retrievedKeys sub =
            case find (\s -> s.id == sub.id) retrievedKeys of
                Nothing ->
                    sub

                Just found ->
                    found

        mergedKeys : List Subscription -> List Subscription
        mergedKeys subscriptions =
            subscriptions
                |> List.map (getRetrievedOrUnretrieved retrievedKeys)
    in
    div [ class "row m0" ]
        [ h4 [ class "mb15" ]
            [ strong
                []
                [ text "API Keys" ]
            ]
        , div
            []
            (List.map
                (viewSubscription
                    model
                )
                (mergedKeys
                    subscriptions
                )
            )
        ]


viewSubscription : Model -> Subscription -> Html Msg
viewSubscription model subscription =
    div []
        [ p [ class "mb5" ]
            [ strong [] [ text subscription.name ]
            , button
                [ class "btn btn-sm"
                , onClick (ShowApiKey subscription.id)
                ]
                [ i [ class "fa fa-eye" ] []
                ]
            , p [ class "obfuscate" ]
                [ viewApiKey subscription
                ]
            ]
        ]


viewApiKey : Subscription -> Html Msg
viewApiKey subscription =
    case subscription.key of
        Nothing ->
            text "XXXXXXXXXXXXXXXXXXXXXXXXXXXX"

        Just key ->
            text key


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
                    a [ AppRoutes.href route, class "btn btn-sm" ]
                        [ i [ class "fa fa-plus" ] []
                        , text (" Add " ++ String.toLower thing)
                        ]
    in
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-6 pl10" ]
                    [ h4 [] [ strong [] [ text ("Recent " ++ thing ++ "s") ] ]
                    ]
                , div [ class "col-sm-6 right" ]
                    [ a [ AppRoutes.href linkRoute, class "btn secondary btn-sm mr10" ] [ text ("View All " ++ thing ++ "s") ]
                    , addButton addRoute
                    ]
                ]
            , hr [ class "mt10" ] []
            , view
            ]
        ]
