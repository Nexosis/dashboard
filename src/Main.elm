module Main exposing (..)

import AppRoutes exposing (Route)
import Data.Config exposing (Config, NexosisToken)
import Data.Context exposing (ContextModel)
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
import Page.Imports as Imports
import Page.ModelDetail as ModelDetail
import Page.Models as Models
import Page.NotFound as NotFound
import Page.SessionDetail as SessionDetail
import Page.SessionStart as SessionStart
import Page.Sessions as Sessions
import Ports
import Request.Log as Log
import Request.Token as Token
import StateStorage as AppState exposing (Msg, loadAppState)
import Task
import Time
import Time.DateTime as DateTime
import Util exposing ((=>))
import View.Page as Page


---- MODEL ----


type Model
    = Initialized App
    | InitializationError String


type alias App =
    { page : Page
    , config : Config
    , error : Maybe Http.Error
    , lastRequest : String
    , lastResponse : Maybe Response.Response
    , messages : List Response.GlobalMessage
    , enabledFeatures : List Feature
    , context : ContextModel
    }


type Msg
    = SetRoute (Maybe AppRoutes.Route)
    | HomeMsg Home.Msg
    | DataSetsMsg DataSets.Msg
    | DataSetDetailMsg DataSetDetail.Msg
    | DataSetAddMsg DataSetAdd.Msg
    | ImportsMsg Imports.Msg
    | SessionsMsg Sessions.Msg
    | SessionDetailMsg SessionDetail.Msg
    | SessionStartMsg SessionStart.Msg
    | ModelsMsg Models.Msg
    | ResponseReceived (Result String Response.Response)
    | CheckToken Time.Time
    | RenewToken (Result Http.Error NexosisToken)
    | ModelDetailMsg ModelDetail.Msg
    | OnAppStateLoaded AppState.Msg


type Page
    = Blank
    | NotFound
    | Error PageLoadError
    | Home Home.Model
    | DataSets DataSets.Model
    | DataSetDetail DataSetDetail.Model
    | DataSetAdd DataSetAdd.Model
    | Imports Imports.Model
    | Sessions Sessions.Model
    | SessionDetail SessionDetail.Model
    | SessionStart SessionStart.Model
    | Models Models.Model
    | ModelDetail ModelDetail.Model



---- UPDATE ----


getQuotas : Maybe Response.Response -> Maybe Response.Quotas
getQuotas resp =
    case resp of
        Nothing ->
            Nothing

        Just resp ->
            Just resp.quotas


setRoute : Maybe AppRoutes.Route -> App -> ( App, Cmd Msg )
setRoute route app =
    case app.config.token of
        Nothing ->
            app => Navigation.load app.config.loginUrl

        Just _ ->
            let
                enabled =
                    isEnabled app.enabledFeatures
            in
            case route of
                Nothing ->
                    -- TODO Load 404 page not found
                    ( app, Cmd.none )

                Just AppRoutes.Home ->
                    let
                        ( pageModel, initCmd ) =
                            Home.init app.config (getQuotas app.lastResponse)
                    in
                    ( { app | page = Home pageModel }, Cmd.map HomeMsg initCmd )

                Just AppRoutes.DataSets ->
                    let
                        ( pageModel, initCmd ) =
                            DataSets.init app.config
                    in
                    { app | page = DataSets pageModel } => Cmd.map DataSetsMsg initCmd

                Just (AppRoutes.DataSetDetail name) ->
                    let
                        ( pageModel, initCmd ) =
                            DataSetDetail.init app.config name
                    in
                    { app | page = DataSetDetail pageModel } => Cmd.map DataSetDetailMsg initCmd

                Just AppRoutes.DataSetAdd ->
                    let
                        ( pageModel, initCmd ) =
                            DataSetAdd.init app.config
                    in
                    { app | page = DataSetAdd pageModel } => Cmd.map DataSetAddMsg initCmd

                Just AppRoutes.Imports ->
                    let
                        ( pageModel, initCmd ) =
                            Imports.init app.config
                    in
                    ( { app | page = Imports pageModel }, Cmd.map ImportsMsg initCmd )

                Just AppRoutes.Sessions ->
                    let
                        ( pageModel, initCmd ) =
                            Sessions.init app.config
                    in
                    ( { app | page = Sessions pageModel }, Cmd.map SessionsMsg initCmd )

                Just (AppRoutes.SessionDetail id) ->
                    let
                        ( pageModel, initCmd ) =
                            SessionDetail.init app.config id
                    in
                    ( { app | page = SessionDetail pageModel }, Cmd.map SessionDetailMsg initCmd )

                Just (AppRoutes.SessionStart dataSetName) ->
                    let
                        ( pageModel, initCmd ) =
                            SessionStart.init app.config dataSetName
                    in
                    ( { app | page = SessionStart pageModel }, Cmd.map SessionStartMsg initCmd )

                Just AppRoutes.Models ->
                    let
                        ( pageModel, initCmd ) =
                            Models.init app.config
                    in
                    ( { app | page = Models pageModel }, Cmd.map ModelsMsg initCmd )

                Just (AppRoutes.ModelDetail id predict) ->
                    let
                        ( pageModel, initCmd ) =
                            ModelDetail.init app.config id predict
                    in
                    ( { app | page = ModelDetail pageModel }, Cmd.map ModelDetailMsg initCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Initialized app ->
            Tuple.mapFirst Initialized <| updatePage app.page msg app

        InitializationError err ->
            let
                e =
                    Debug.log "Init error" err
            in
            model => (Log.logMessage <| Log.LogMessage ("Error initializing app: " ++ err) Log.Error)


updatePage : Page -> Msg -> App -> ( App, Cmd Msg )
updatePage page msg app =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
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
            in
            { app
                | lastResponse = Just response
                , messages = app.messages ++ response.messages
            }
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
            case app.config.token of
                Just nexosisToken ->
                    if Jwt.isExpired hourFromNow nexosisToken.rawToken |> Result.toMaybe |> Maybe.withDefault True then
                        let
                            renewTokenRequest =
                                Token.renewAccessToken app.config
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
                    app.config

                newConfig =
                    { config | token = Just newToken }
            in
            { app | config = newConfig } => Cmd.none

        ( RenewToken (Err err), _ ) ->
            app
                --todo - Log needs to finish first, try re-writing with tasks.
                => Cmd.batch
                    [ Navigation.load app.config.loginUrl, Log.logMessage <| Log.LogMessage ("Error during token renewal " ++ toString err) Log.Error ]

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
            let
                e =
                    Debug.log "err " err
            in
            Error.pageLoadError Page.Home
                """
            Sorry, it seems we are having an issues starting the application.
            Try checking your internet connection and refreshing the page.
            """
                |> Error.view
                |> Page.basicLayout Page.Other

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
                    Home.view subModel
                        |> layout Page.Home
                        |> Html.map HomeMsg

                DataSets subModel ->
                    DataSets.view subModel
                        |> layout Page.DataSets
                        |> Html.map DataSetsMsg

                DataSetDetail subModel ->
                    DataSetDetail.view subModel
                        |> layout Page.DataSetData
                        |> Html.map DataSetDetailMsg

                DataSetAdd subModel ->
                    DataSetAdd.view subModel
                        |> layout Page.DataSetAdd
                        |> Html.map DataSetAddMsg

                Imports subModel ->
                    Imports.view subModel
                        |> layout Page.Imports
                        |> Html.map ImportsMsg

                Sessions subModel ->
                    Sessions.view subModel
                        |> layout Page.Sessions
                        |> Html.map SessionsMsg

                SessionDetail subModel ->
                    SessionDetail.view subModel
                        |> layout Page.SessionDetail
                        |> Html.map SessionDetailMsg

                SessionStart subModel ->
                    SessionStart.view subModel
                        |> layout Page.SessionStart
                        |> Html.map SessionStartMsg

                Models subModel ->
                    Models.view subModel
                        |> layout Page.Models
                        |> Html.map ModelsMsg

                ModelDetail subModel ->
                    ModelDetail.view subModel
                        |> layout Page.ModelDetail
                        |> Html.map ModelDetailMsg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized app ->
            Sub.batch
                [ Ports.responseReceived (Response.decodeXhrResponse app.config.baseUrl >> ResponseReceived)
                , Time.every Time.minute CheckToken
                , pageSubscriptions app.page
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
        Ok appContext ->
            let
                ( app, cmd ) =
                    setRoute (AppRoutes.fromLocation location)
                        appContext
            in
            Initialized app
                => Cmd.batch [ cmd, Task.perform CheckToken Time.now, loadAppState ]

        Err error ->
            ( InitializationError error, Cmd.none )


flagsDecoder : Decode.Decoder App
flagsDecoder =
    Pipeline.decode App
        |> Pipeline.hardcoded initialPage
        |> Pipeline.custom Data.Config.configDecoder
        |> Pipeline.hardcoded Nothing
        |> Pipeline.hardcoded ""
        |> Pipeline.hardcoded Nothing
        |> Pipeline.hardcoded []
        |> Pipeline.required "enabledFeatures" (Decode.list Feature.featureDecoder)
        |> Pipeline.hardcoded (ContextModel 10)


main : Program Value Model Msg
main =
    Navigation.programWithFlags (AppRoutes.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
