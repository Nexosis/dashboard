module Main exposing (..)

import AppRoutes exposing (Route)
import Data.Config exposing (Config, NexosisToken)
import Data.Context exposing (ContextModel, defaultContext)
import Data.Message as Message
import Data.Metric exposing (Metric)
import Data.Response as Response
import Feature exposing (Feature, isEnabled)
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Value, decodeValue)
import Json.Decode.Pipeline as Pipeline
import Jwt
import Navigation exposing (Location)
import Page.DataSetAdd as DataSetAdd
import Page.DataSetDetail as DataSetDetail
import Page.DataSets as DataSets
import Page.Error as Error exposing (PageLoadError)
import Page.Home as Home
import Page.ModelDetail as ModelDetail
import Page.Models as Models
import Page.NotFound as NotFound
import Page.SessionDetail as SessionDetail
import Page.SessionStart as SessionStart
import Page.Sessions as Sessions
import Ports
import RemoteData as Remote
import Request.Log as Log
import Request.Metric as Metrics
import Request.Token as Token
import StateStorage exposing (Msg(OnAppStateLoaded), appStateLoaded, loadAppState, updateContext)
import Task
import Time
import Time.DateTime as DateTime
import Util exposing ((=>))
import View.Page as Page


---- MODEL ----


type Model
    = ConfigurationLoaded InitState
    | Initialized App
    | InitializationError String


type alias InitState =
    { config : Config
    , route : Maybe ( Route, String )
    , enabledFeatures : List Feature
    }


type alias App =
    { page : Page
    , error : Maybe Http.Error
    , lastRequest : String
    , lastResponse : Maybe Response.Response
    , messages : List Response.GlobalMessage
    , enabledFeatures : List Feature
    , context : ContextModel
    }


type Msg
    = SetRoute (Maybe ( AppRoutes.Route, String ))
    | HomeMsg Home.Msg
    | DataSetsMsg DataSets.Msg
    | DataSetDetailMsg DataSetDetail.Msg
    | DataSetAddMsg DataSetAdd.Msg
    | SessionsMsg Sessions.Msg
    | SessionDetailMsg SessionDetail.Msg
    | SessionStartMsg SessionStart.Msg
    | ModelsMsg Models.Msg
    | ResponseReceived (Result String Response.Response)
    | CheckToken Time.Time
    | RenewToken (Result Http.Error NexosisToken)
    | ModelDetailMsg ModelDetail.Msg
    | OnAppStateLoaded StateStorage.Msg
    | OnAppStateUpdated StateStorage.Msg
    | MetricTextLoaded (Remote.WebData (List Metric))


type Page
    = Blank
    | NotFound
    | Error PageLoadError
    | Home Home.Model
    | DataSets DataSets.Model
    | DataSetDetail DataSetDetail.Model
    | DataSetAdd DataSetAdd.Model
    | Sessions Sessions.Model
    | SessionDetail SessionDetail.Model
    | SessionStart SessionStart.Model
    | Models Models.Model
    | ModelDetail ModelDetail.Model



---- UPDATE ----


getMetrics : ContextModel -> Cmd Msg
getMetrics context =
    Metrics.get context.config
        |> Remote.sendRequest
        |> Cmd.map MetricTextLoaded


getQuotas : Maybe Response.Response -> Maybe Response.Quotas
getQuotas resp =
    case resp of
        Nothing ->
            Nothing

        Just resp ->
            Just resp.quotas


setRoute : Maybe ( AppRoutes.Route, String ) -> App -> ( App, Cmd Msg )
setRoute route app =
    case app.context.config.token of
        Nothing ->
            app => Navigation.load app.context.config.loginUrl

        Just _ ->
            let
                enabled =
                    isEnabled app.enabledFeatures
            in
            case route of
                Nothing ->
                    -- TODO Load 404 page not found
                    ( app, Cmd.none )

                Just ( AppRoutes.Home, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            Home.init app.context.config (getQuotas app.lastResponse)
                    in
                    { app | page = Home pageModel } => Cmd.batch [ Cmd.map HomeMsg initCmd, Ports.setPageTitle title ]

                Just ( AppRoutes.DataSets, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            DataSets.init app.context
                    in
                    { app | page = DataSets pageModel } => Cmd.batch [ Cmd.map DataSetsMsg initCmd, Ports.setPageTitle title ]

                Just ( AppRoutes.DataSetDetail name, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            DataSetDetail.init app.context.config name
                    in
                    { app | page = DataSetDetail pageModel } => Cmd.batch [ Cmd.map DataSetDetailMsg initCmd, Ports.setPageTitle title ]

                Just ( AppRoutes.DataSetAdd, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            DataSetAdd.init app.context.config
                    in
                    { app | page = DataSetAdd pageModel } => Cmd.batch [ Cmd.map DataSetAddMsg initCmd, Ports.setPageTitle title ]

                Just ( AppRoutes.Sessions, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            Sessions.init app.context
                    in
                    { app | page = Sessions pageModel } => Cmd.batch [ Cmd.map SessionsMsg initCmd, Ports.setPageTitle title ]

                Just ( AppRoutes.SessionDetail id, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            SessionDetail.init app.context id
                    in
                    { app | page = SessionDetail pageModel } => Cmd.batch [ Cmd.map SessionDetailMsg initCmd, Ports.setPageTitle title ]

                Just ( AppRoutes.SessionStart dataSetName, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            SessionStart.init app.context.config dataSetName
                    in
                    { app | page = SessionStart pageModel } => Cmd.batch [ Cmd.map SessionStartMsg initCmd, Ports.setPageTitle title ]

                Just ( AppRoutes.Models, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            Models.init app.context
                    in
                    { app | page = Models pageModel } => Cmd.batch [ Cmd.map ModelsMsg initCmd, Ports.setPageTitle title ]

                Just ( AppRoutes.ModelDetail id, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            ModelDetail.init app.context id
                    in
                    { app | page = ModelDetail pageModel } => Cmd.batch [ Cmd.map ModelDetailMsg initCmd, Ports.setPageTitle title ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfigurationLoaded initState ->
            case msg of
                OnAppStateLoaded ctx ->
                    case ctx of
                        StateStorage.OnAppStateLoaded mbctx ->
                            let
                                newContext =
                                    { mbctx | config = initState.config }

                                app =
                                    App initialPage Nothing "" Nothing [] initState.enabledFeatures newContext

                                ( routedApp, cmd ) =
                                    setRoute initState.route
                                        app
                            in
                            Initialized routedApp => Cmd.batch [ Task.perform CheckToken Time.now, cmd, getMetrics newContext ]

                OnAppStateUpdated ctx ->
                    model => Cmd.none

                _ ->
                    InitializationError
                        "Unknown initialization message"
                        => Cmd.none

        Initialized app ->
            Tuple.mapFirst Initialized <| updatePage app.page msg app

        InitializationError err ->
            model => (Log.logMessage <| Log.LogMessage ("Error initializing app: " ++ err) Log.Error)


updatePage : Page -> Msg -> App -> ( App, Cmd Msg )
updatePage page msg app =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel app.context
            in
            ( { app | page = toModel newModel }, Cmd.map toMsg newCmd )
    in
    case ( msg, page ) of
        -- Update for page transitions
        ( SetRoute route, _ ) ->
            setRoute route app

        -- Update for page specific msgs
        ( HomeMsg subMsg, Home subModel ) ->
            toPage Home HomeMsg Home.update subMsg subModel

        ( DataSetsMsg subMsg, DataSets subModel ) ->
            toPage DataSets DataSetsMsg DataSets.update subMsg subModel

        ( DataSetDetailMsg subMsg, DataSetDetail subModel ) ->
            toPage DataSetDetail DataSetDetailMsg DataSetDetail.update subMsg subModel

        ( DataSetAddMsg subMsg, DataSetAdd subModel ) ->
            toPage DataSetAdd DataSetAddMsg DataSetAdd.update subMsg subModel

        ( ModelsMsg subMsg, Models subModel ) ->
            toPage Models ModelsMsg Models.update subMsg subModel

        ( ModelDetailMsg subMsg, ModelDetail subModel ) ->
            toPage ModelDetail ModelDetailMsg ModelDetail.update subMsg subModel

        ( SessionsMsg subMsg, Sessions subModel ) ->
            toPage Sessions SessionsMsg Sessions.update subMsg subModel

        ( SessionDetailMsg subMsg, SessionDetail subModel ) ->
            toPage SessionDetail SessionDetailMsg SessionDetail.update subMsg subModel

        ( SessionStartMsg subMsg, SessionStart subModel ) ->
            toPage SessionStart SessionStartMsg SessionStart.update subMsg subModel

        ( ResponseReceived (Ok response), _ ) ->
            let
                quotaUpdatedMsg : Home.Msg
                quotaUpdatedMsg =
                    Home.QuotasUpdated (getQuotas (Just response))

                messagesToKeep =
                    response.messages
                        |> List.filter (\m -> m.severity == Message.Warning || m.severity == Message.Error)
                        |> List.filterMap
                            (\m ->
                                if List.member m app.messages then
                                    Nothing
                                else
                                    Just m
                            )
                        |> flip List.append app.messages
                        |> List.take 5
            in
            { app | messages = messagesToKeep }
                => Cmd.batch [ Ports.prismHighlight (), Task.perform HomeMsg (Task.succeed quotaUpdatedMsg) ]

        ( ResponseReceived (Err err), _ ) ->
            { app | lastResponse = Nothing }
                => (Log.logMessage <| Log.LogMessage ("Unable to decode Response " ++ err) Log.Error)

        ( CheckToken now, _ ) ->
            let
                hourFromNow =
                    now
                        |> DateTime.fromTimestamp
                        |> DateTime.addHours 1
                        |> DateTime.toTimestamp
            in
            case app.context.config.token of
                Just nexosisToken ->
                    if Jwt.isExpired hourFromNow nexosisToken.rawToken |> Result.toMaybe |> Maybe.withDefault True then
                        let
                            renewTokenRequest =
                                Token.renewAccessToken app.context.config
                                    |> Http.send RenewToken
                        in
                        app => renewTokenRequest
                    else
                        app => Cmd.none

                _ ->
                    app => Cmd.none

        ( RenewToken (Ok newToken), _ ) ->
            let
                config =
                    app.context.config

                newConfig =
                    { config | token = Just newToken }

                context =
                    app.context

                newContext =
                    { context | config = newConfig }
            in
            { app | context = newContext } => Cmd.none

        ( RenewToken (Err err), _ ) ->
            app
                --todo - Log needs to finish first, try re-writing with tasks.
                => Cmd.batch
                    [ Navigation.load app.context.config.loginUrl, Log.logMessage <| Log.LogMessage ("Error during token renewal " ++ toString err) Log.Error ]

        ( OnAppStateUpdated externalMsg, _ ) ->
            case externalMsg of
                StateStorage.OnAppStateLoaded mbctx ->
                    let
                        existingConfig =
                            app.context.config

                        newContext =
                            { mbctx | config = existingConfig }

                        newApp =
                            { app | context = newContext }
                    in
                    newApp => Cmd.none

        ( MetricTextLoaded response, _ ) ->
            case response of
                Remote.Success metrics ->
                    let
                        context =
                            app.context

                        newContext =
                            { context | metricExplainers = metrics }
                    in
                    { app | context = newContext } => Cmd.none

                _ ->
                    app => Cmd.none

        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            app => Cmd.none

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            app => Cmd.none



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        InitializationError err ->
            Error.pageLoadError Page.Home
                """
            Sorry, it seems we are having an issues starting the application.
            Try checking your internet connection and refreshing the page.
            """
                |> Error.view
                |> Page.basicLayout Page.Other

        ConfigurationLoaded initState ->
            Html.text ""
                |> Page.emptyLayout Page.Other

        Initialized app ->
            let
                layout =
                    Page.layoutShowingResponses app
            in
            case app.page of
                NotFound ->
                    layout Page.Other NotFound.view

                Blank ->
                    -- This is for the very initial page load, while we are loading
                    -- data via HTTP. We could also render a spinner here.
                    Html.text ""
                        |> layout Page.Other

                Error subModel ->
                    Error.view subModel
                        |> layout Page.Other

                Home subModel ->
                    Home.view subModel app.context app.messages
                        |> layout Page.Home
                        |> Html.map HomeMsg

                DataSets subModel ->
                    DataSets.view subModel app.context
                        |> layout Page.DataSets
                        |> Html.map DataSetsMsg

                DataSetDetail subModel ->
                    DataSetDetail.view subModel app.context
                        |> layout Page.DataSetData
                        |> Html.map DataSetDetailMsg

                DataSetAdd subModel ->
                    DataSetAdd.view subModel app.context
                        |> layout Page.DataSetAdd
                        |> Html.map DataSetAddMsg

                Sessions subModel ->
                    Sessions.view subModel app.context
                        |> layout Page.Sessions
                        |> Html.map SessionsMsg

                SessionDetail subModel ->
                    SessionDetail.view subModel app.context
                        |> layout Page.SessionDetail
                        |> Html.map SessionDetailMsg

                SessionStart subModel ->
                    SessionStart.view subModel app.context
                        |> layout Page.SessionStart
                        |> Html.map SessionStartMsg

                Models subModel ->
                    Models.view subModel app.context
                        |> layout Page.Models
                        |> Html.map ModelsMsg

                ModelDetail subModel ->
                    ModelDetail.view subModel app.context
                        |> layout Page.ModelDetail
                        |> Html.map ModelDetailMsg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ConfigurationLoaded initState ->
            Sub.map OnAppStateLoaded (appStateLoaded initState.config)

        Initialized app ->
            Sub.batch
                [ Ports.responseReceived (Response.decodeXhrResponse app.context.config.baseUrl >> ResponseReceived)
                , Time.every Time.minute CheckToken
                , pageSubscriptions app.page
                , Sub.map OnAppStateUpdated (appStateLoaded app.context.config)
                ]

        InitializationError _ ->
            Sub.none


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        DataSetAdd subModel ->
            DataSetAdd.subscriptions subModel
                |> Sub.map DataSetAddMsg

        ModelDetail subModel ->
            ModelDetail.subscriptions subModel
                |> Sub.map ModelDetailMsg

        SessionStart subModel ->
            SessionStart.subscriptions subModel
                |> Sub.map SessionStartMsg

        DataSetDetail subModel ->
            DataSetDetail.subscriptions subModel
                |> Sub.map DataSetDetailMsg

        _ ->
            Sub.none



---- PROGRAM ----


initialPage : Page
initialPage =
    Blank


init : Value -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        flagDecodeResult =
            decodeValue flagsDecoder flags
    in
    case flagDecodeResult of
        Ok initState ->
            let
                route =
                    AppRoutes.fromLocation location
            in
            ConfigurationLoaded { initState | route = route }
                => loadAppState

        Err error ->
            ( InitializationError error, Cmd.none )


flagsDecoder : Decode.Decoder InitState
flagsDecoder =
    Pipeline.decode InitState
        |> Pipeline.custom Data.Config.configDecoder
        |> Pipeline.hardcoded Nothing
        |> Pipeline.required "enabledFeatures" (Decode.list Feature.featureDecoder)


main : Program Value Model Msg
main =
    Navigation.programWithFlags (AppRoutes.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
