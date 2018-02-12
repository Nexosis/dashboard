module Main exposing (..)

import Data.Config exposing (Config)
import Data.Response as Response
import Feature exposing (Feature, isEnabled)
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Value, decodeValue)
import Json.Decode.Pipeline as Pipeline
import Jwt
import Navigation exposing (Location)
import Page.DataSetDetail as DataSetDetail
import Page.DataSets as DataSets
import Page.Error as Error exposing (PageLoadError)
import Page.Home as Home
import Page.Imports as Imports
import Page.Models as Models
import Page.NotFound as NotFound
import Page.Sessions as Sessions
import Ports
import Request.Log as Log
import Route exposing (Route)
import Time
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
    , messages : List Response.Message
    , enabledFeatures : List Feature
    }


type Page
    = Blank
    | NotFound
    | Error PageLoadError
    | Home Home.Model
    | DataSets DataSets.Model
    | DataSetDetail DataSetDetail.Model
    | Imports Imports.Model
    | Sessions Sessions.Model
    | Models Models.Model



---- UPDATE ----


type Msg
    = SetRoute (Maybe Route)
    | HomeMsg Home.Msg
    | DataSetsMsg DataSets.Msg
    | DataSetDetailMsg DataSetDetail.Msg
    | ImportsMsg Imports.Msg
    | SessionsMsg Sessions.Msg
    | ModelsMsg Models.Msg
    | ResponseReceived (Result String Response.Response)
    | CheckToken Time.Time


setRoute : Maybe Route -> App -> ( App, Cmd Msg )
setRoute route app =
    let
        enabled =
            isEnabled app.enabledFeatures
    in
    case route of
        Nothing ->
            -- TODO Load 404 page not found
            ( app, Cmd.none )

        Just Route.Home ->
            let
                pageModel =
                    Home (Home.init enabled)
            in
            ( { app | page = pageModel }, Cmd.none )

        Just (Route.DataSetRoute dataSetRoute) ->
            case dataSetRoute of
                Route.DataSets ->
                    let
                        ( pageModel, initCmd ) =
                            DataSets.init app.config
                    in
                    { app | page = DataSets pageModel } => Cmd.map DataSetsMsg initCmd

                Route.DataSetDetail name ->
                    let
                        ( pageModel, initCmd ) =
                            DataSetDetail.init app.config name
                    in
                    { app | page = DataSetDetail pageModel } => Cmd.map DataSetDetailMsg initCmd

        Just Route.Imports ->
            let
                ( pageModel, initCmd ) =
                    Imports.init app.config
            in
            ( { app | page = Imports pageModel }, Cmd.map ImportsMsg initCmd )

        Just (Route.SessionsRoute sessionRoute) ->
            case sessionRoute of
                Route.Sessions ->
                    let
                        ( pageModel, initCmd ) =
                            Sessions.init app.config
                    in
                    ( { app | page = Sessions pageModel }, Cmd.map SessionsMsg initCmd )

                Route.SessionDetail id ->
                    --todo: change this route to point to an individual session.
                    let
                        ( pageModel, initCmd ) =
                            Sessions.init app.config
                    in
                    ( { app | page = Sessions pageModel }, Cmd.map SessionsMsg initCmd )

        Just Route.Models ->
            let
                ( pageModel, initCmd ) =
                    Models.init app.config
            in
            ( { app | page = Models pageModel }, Cmd.map ModelsMsg initCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
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

        ( ResponseReceived (Ok response), _ ) ->
            { app
                | lastResponse = Just response
                , messages = app.messages ++ response.messages
            }
                => Ports.prismHighlight ()

        ( ResponseReceived (Err err), _ ) ->
            { app | lastResponse = Nothing }
                => (Log.logMessage <| Log.LogMessage ("Unable to decode Response " ++ err) Log.Error)

        ( CheckToken time, _ ) ->
            if Jwt.isExpired time app.config.rawToken |> Result.toMaybe |> Maybe.withDefault True then
                let
                    s =
                        Debug.log "Token is expired" time
                in
                app => Cmd.none
                -- TODO: renew the token somehow
            else
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
        InitializationError _ ->
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

                Imports subModel ->
                    Imports.view subModel
                        |> layout Page.Imports
                        |> Html.map ImportsMsg

                Sessions subModel ->
                    Sessions.view subModel
                        |> layout Page.Sessions
                        |> Html.map SessionsMsg

                Models subModel ->
                    Models.view subModel
                        |> layout Page.Models
                        |> Html.map ModelsMsg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized app ->
            Sub.batch
                [ Ports.responseReceived (Response.decodeXhrResponse app.config.baseUrl >> ResponseReceived)
                , Time.every Time.minute CheckToken
                ]

        InitializationError _ ->
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
            Tuple.mapFirst Initialized <|
                setRoute (Route.fromLocation location)
                    appContext

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


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
