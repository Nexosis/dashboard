module Main exposing (..)

import AppRoutes exposing (Route)
import Data.Config exposing (Config)
import Data.Context as Context exposing (ContextModel, TokenResponse, UserAuth, contextToAuth)
import Data.Response as Response exposing (Quotas)
import Dom.Scroll as Scroll exposing (toTop)
import Feature exposing (Feature, isEnabled)
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Value, decodeValue)
import Json.Decode.Pipeline as Pipeline
import Jwt
import Navigation exposing (Location)
import Nexosis.Api.Metrics exposing (Metric)
import Nexosis.Types.Message as Message
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
import Request.Token as Token
import StateStorage exposing (Msg(OnAppStateLoaded), appStateLoaded, loadAppState, updateContext)
import Task
import Time
import Time.DateTime as DateTime
import Util exposing ((=>), delayTask)
import View.Error as ErrorView
import View.Page as Page


---- MODEL ----


type Model
    = ConfigurationLoaded ( InitState, UserAuth )
    | Initialized App
    | InitializationError InitError


type alias InitState =
    { config : Config
    , route : Maybe ( Route, String )
    , enabledFeatures : List Feature
    }


type InitError
    = UserMessage String String
    | GenericMessage String


type alias App =
    { page : Page
    , messages : List Response.GlobalMessage
    , enabledFeatures : List Feature
    , context : ContextModel
    , pageLoadFailed : Maybe ( Page, Http.Error )
    }


type alias Loading a b =
    { a | loadingResponse : Remote.WebData b }


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
    | RenewToken (Result Http.Error TokenResponse)
    | ModelDetailMsg ModelDetail.Msg
    | OnAppStateLoaded StateStorage.Msg
    | OnAppStateUpdated StateStorage.Msg
    | PageLoadFailed Http.Error
    | MetricTextLoaded (Remote.WebData (List Metric))
    | NoOp


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


getMetricsTask : ContextModel -> Task.Task Http.Error (List Metric)
getMetricsTask context =
    Nexosis.Api.Metrics.get (contextToAuth context) |> Http.toTask


getQuotas : Maybe Response.Response -> Maybe Response.Quotas
getQuotas resp =
    case resp of
        Nothing ->
            Nothing

        Just resp ->
            Just resp.quotas


setRoute : Maybe ( AppRoutes.Route, String ) -> App -> ( App, Cmd Msg )
setRoute route app =
    let
        enabled =
            isEnabled app.enabledFeatures

        scrollCmd =
            Task.attempt (always NoOp) <| Scroll.toTop "html"

        ( ( newApp, cmd ), title ) =
            case route of
                Nothing ->
                    -- TODO Load 404 page not found
                    ( app, Cmd.none ) => "Not Found"

                Just ( AppRoutes.Home, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            Home.init app.context
                    in
                    { app | page = Home pageModel } => Cmd.map HomeMsg initCmd => title

                Just ( AppRoutes.DataSets, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            DataSets.init app.context
                    in
                    { app | page = DataSets pageModel } => Cmd.map DataSetsMsg initCmd => title

                Just ( AppRoutes.DataSetDetail name, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            DataSetDetail.init app.context name
                    in
                    { app | page = DataSetDetail pageModel } => Cmd.map DataSetDetailMsg initCmd => title

                Just ( AppRoutes.DataSetAdd, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            DataSetAdd.init app.context.config
                    in
                    { app | page = DataSetAdd pageModel } => Cmd.map DataSetAddMsg initCmd => title

                Just ( AppRoutes.Sessions, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            Sessions.init app.context
                    in
                    { app | page = Sessions pageModel } => Cmd.map SessionsMsg initCmd => title

                Just ( AppRoutes.SessionDetail id, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            SessionDetail.init app.context id
                    in
                    { app | page = SessionDetail pageModel } => Cmd.map SessionDetailMsg initCmd => title

                Just ( AppRoutes.SessionStart dataSetName, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            SessionStart.init app.context dataSetName
                    in
                    { app | page = SessionStart pageModel } => Cmd.map SessionStartMsg initCmd => title

                Just ( AppRoutes.Models, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            Models.init app.context
                    in
                    { app | page = Models pageModel } => Cmd.map ModelsMsg initCmd => title

                Just ( AppRoutes.ModelDetail id, title ) ->
                    let
                        ( pageModel, initCmd ) =
                            ModelDetail.init app.context id
                    in
                    { app | page = ModelDetail pageModel } => Cmd.map ModelDetailMsg initCmd => title
    in
    newApp => Cmd.batch [ cmd, Ports.setPageTitle title, scrollCmd ]


extractError : { b | loadingError : Maybe Http.Error } -> Maybe Http.Error
extractError { loadingError } =
    loadingError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfigurationLoaded ( initState, auth ) ->
            case msg of
                OnAppStateLoaded ctx ->
                    case ctx of
                        StateStorage.OnAppStateLoaded localStorage ->
                            let
                                newContext =
                                    { localStorage = localStorage
                                    , config = initState.config
                                    , metricExplainers = []
                                    , quotas = Nothing
                                    , auth = auth
                                    }

                                app =
                                    App initialPage [] initState.enabledFeatures newContext Nothing

                                ( routedApp, cmd ) =
                                    setRoute initState.route
                                        app

                                headerValues =
                                    { userName = Context.getUserName newContext |> Maybe.withDefault "Account"
                                    , overviewLink = newContext.config.accountSiteUrl ++ "/apiaccount/accountstatus"
                                    , referLink = newContext.config.accountSiteUrl ++ "/apiaccount/referAFriend"
                                    , logoutLink = newContext.config.accountSiteUrl ++ "/account/logout?returnUrl=" ++ newContext.config.apiManagerUrl ++ "/signout"
                                    }

                                getMetrics =
                                    getMetricsTask newContext
                                        |> Remote.asCmd
                                        |> Cmd.map MetricTextLoaded
                            in
                            Initialized routedApp => Cmd.batch [ Task.perform CheckToken Time.now, cmd, getMetrics, Ports.setHeaderValues headerValues ]

                _ ->
                    InitializationError (GenericMessage "Unknown initialization message") => Cmd.none

        Initialized app ->
            Tuple.mapFirst Initialized <| updatePage app.page msg app

        InitializationError err ->
            let
                logMessage =
                    case err of
                        GenericMessage error ->
                            error

                        UserMessage _ error ->
                            error
            in
            model => (Log.logMessage <| Log.LogMessage ("Error initializing app: " ++ logMessage) Log.Error)


updatePage : Page -> Msg -> App -> ( App, Cmd Msg )
updatePage page msg app =
    let
        doUpdate subUpdate subMsg subModel =
            subUpdate subMsg subModel app.context

        toPage toModel toMsg ( newModel, newCmd ) =
            ( { app | page = toModel newModel }, Cmd.map toMsg newCmd )

        resetError app =
            { app | pageLoadFailed = Nothing }

        toPageOrError toModel toMsg ( newModel, newCmd ) =
            case newModel.loadingResponse of
                Remote.Failure err ->
                    ( app, Task.perform PageLoadFailed <| Task.succeed err )

                _ ->
                    toPage toModel toMsg ( newModel, newCmd )
    in
    case ( msg, page ) of
        ( PageLoadFailed err, _ ) ->
            ( { app | pageLoadFailed = Just <| ( page, err ) }, Cmd.none )

        -- Update for page transitions
        ( SetRoute route, _ ) ->
            setRoute route <| resetError app

        -- Update for page specific msgs
        ( HomeMsg subMsg, Home subModel ) ->
            toPage Home HomeMsg <| doUpdate Home.update subMsg subModel

        ( DataSetsMsg subMsg, DataSets subModel ) ->
            toPage DataSets DataSetsMsg <| doUpdate DataSets.update subMsg subModel

        ( DataSetDetailMsg subMsg, DataSetDetail subModel ) ->
            toPageOrError DataSetDetail DataSetDetailMsg <| doUpdate DataSetDetail.update subMsg subModel

        ( DataSetAddMsg subMsg, DataSetAdd subModel ) ->
            toPage DataSetAdd DataSetAddMsg <| doUpdate DataSetAdd.update subMsg subModel

        ( ModelsMsg subMsg, Models subModel ) ->
            toPage Models ModelsMsg <| doUpdate Models.update subMsg subModel

        ( ModelDetailMsg subMsg, ModelDetail subModel ) ->
            toPageOrError ModelDetail ModelDetailMsg <| doUpdate ModelDetail.update subMsg subModel

        ( SessionsMsg subMsg, Sessions subModel ) ->
            toPage Sessions SessionsMsg <| doUpdate Sessions.update subMsg subModel

        ( SessionDetailMsg subMsg, SessionDetail subModel ) ->
            toPageOrError SessionDetail SessionDetailMsg <| doUpdate SessionDetail.update subMsg subModel

        ( SessionStartMsg subMsg, SessionStart subModel ) ->
            toPage SessionStart SessionStartMsg <| doUpdate SessionStart.update subMsg subModel

        ( ResponseReceived (Ok response), _ ) ->
            let
                setQuotas context =
                    { context | quotas = getQuotas (Just response) }

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
            { app | messages = messagesToKeep, context = setQuotas app.context } => Cmd.none

        ( ResponseReceived (Err err), _ ) ->
            app => (Log.logMessage <| Log.LogMessage ("Unable to decode Response " ++ err) Log.Error)

        ( CheckToken now, _ ) ->
            let
                hourFromNow =
                    now
                        |> DateTime.fromTimestamp
                        |> DateTime.addHours 1
                        |> DateTime.toTimestamp
            in
            case Context.getRawToken app.context of
                Just nexosisToken ->
                    if Jwt.isExpired hourFromNow nexosisToken |> Result.toMaybe |> Maybe.withDefault True then
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
                newContext =
                    Context.setNewToken app.context newToken
            in
            { app | context = newContext } => Cmd.none

        ( RenewToken (Err err), _ ) ->
            app
                --todo - Log needs to finish first, try re-writing with tasks.
                => Cmd.batch
                    [ Navigation.load app.context.config.loginUrl, Log.logMessage <| Log.LogMessage ("Error during token renewal " ++ toString err) Log.Error ]

        ( OnAppStateUpdated externalMsg, _ ) ->
            case externalMsg of
                StateStorage.OnAppStateLoaded localStorage ->
                    let
                        existingContext =
                            app.context

                        newContext =
                            { existingContext | localStorage = localStorage }
                    in
                    { app | context = newContext } => Cmd.none

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

                Remote.Failure err ->
                    case err of
                        Http.NetworkError ->
                            let
                                delayMetricsCall =
                                    delayTask 60
                                        |> Task.andThen (\_ -> getMetricsTask app.context)
                                        |> Remote.asCmd
                                        |> Cmd.map MetricTextLoaded
                            in
                            app => delayMetricsCall

                        _ ->
                            app => Cmd.none

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
    let
        layout =
            Page.layout
    in
    case model of
        InitializationError err ->
            let
                errorMessage =
                    case err of
                        GenericMessage _ ->
                            """
                            Sorry, it seems we are having an issues starting the application.
                            Try checking your internet connection and refreshing the page.
                            """

                        UserMessage message _ ->
                            message
            in
            Error.pageLoadError Page.Home errorMessage
                |> Error.view
                |> layout Page.Other

        ConfigurationLoaded initState ->
            Html.text ""
                |> layout Page.Other

        Initialized app ->
            case app.pageLoadFailed of
                Just ( page, err ) ->
                    ErrorView.viewHttpError err |> Error.viewError |> layout Page.Other

                Nothing ->
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
        ConfigurationLoaded ( initState, _ ) ->
            Sub.map OnAppStateLoaded appStateLoaded

        Initialized app ->
            let
                subs =
                    [ Ports.responseReceived (Response.decodeXhrResponse (Context.getBaseUrl app.context) >> ResponseReceived)
                    , pageSubscriptions app.page
                    , Sub.map OnAppStateUpdated appStateLoaded
                    ]

                allSubs =
                    if Context.isTokenAuth app.context then
                        Time.every Time.minute CheckToken :: subs
                    else
                        subs
            in
            Sub.batch allSubs

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
        configDecodeResult =
            decodeValue flagsDecoder flags

        authResult =
            decodeValue Context.decodeUserAuth flags
    in
    case ( configDecodeResult, authResult ) of
        ( Ok config, Ok auth ) ->
            let
                route =
                    AppRoutes.fromLocation location
            in
            ConfigurationLoaded ( { config | route = route }, auth )
                => loadAppState

        ( Ok initState, Err authError ) ->
            if String.startsWith "localhost" location.host then
                InitializationError
                    (UserMessage
                        """
                    We were unable to find a Nexosis Api Key to use.  Please set an environment variable named "NEXOSIS_API_KEY" with your key.
                    """
                        authError
                    )
                    => Cmd.none
            else
                InitializationError (GenericMessage authError) => Navigation.load initState.config.loginUrl

        ( Err error, _ ) ->
            ( InitializationError (GenericMessage error), Cmd.none )


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
