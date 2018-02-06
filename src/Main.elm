module Main exposing (..)

import Data.Config exposing (ApiKey, Config)
import Data.Response as Response
import Html exposing (..)
import Http
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
import Route exposing (..)
import Time
import Util exposing ((=>))
import View.Page as Page exposing (ActivePage)


---- MODEL ----


type alias Model =
    { page : Page
    , config : Config
    , error : Maybe Http.Error
    , lastRequest : String
    , lastResponse : Maybe Response.Response
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


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute route model =
    case route of
        Nothing ->
            -- TODO Load 404 page not found
            ( model, Cmd.none )

        Just Route.Home ->
            ( { model | page = Home Home.init }, Cmd.none )

        Just Route.DataSets ->
            let
                ( pageModel, initCmd ) =
                    DataSets.init model.config
            in
            { model | page = DataSets pageModel } => Cmd.map DataSetsMsg initCmd

        Just (Route.DataSetDetail name) ->
            let
                ( pageModel, initCmd ) =
                    DataSetDetail.init model.config name
            in
            { model | page = DataSetDetail pageModel } => Cmd.map DataSetDetailMsg initCmd

        Just Route.Imports ->
            let
                ( pageModel, initCmd ) =
                    Imports.init model.config
            in
            ( { model | page = Imports pageModel }, Cmd.map ImportsMsg initCmd )

        Just Route.Sessions ->
            let
                ( pageModel, initCmd ) =
                    Sessions.init model.config
            in
            Debug.crash "Sessions aren't working yet."
                ( { model | page = Sessions pageModel }, Cmd.map SessionsMsg initCmd )

        Just Route.Models ->
            let
                ( pageModel, initCmd ) =
                    Models.init model.config
            in
            ( { model | page = Models pageModel }, Cmd.map ModelsMsg initCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | page = toModel newModel }, Cmd.map toMsg newCmd )
    in
    case ( msg, page ) of
        -- Update for page transitions
        ( SetRoute route, _ ) ->
            setRoute route model

        -- Update for page specific msgs
        ( HomeMsg subMsg, Home subModel ) ->
            toPage Home HomeMsg Home.update subMsg subModel

        ( DataSetsMsg subMsg, DataSets subModel ) ->
            toPage DataSets DataSetsMsg DataSets.update subMsg subModel

        ( DataSetDetailMsg subMsg, DataSetDetail subModel ) ->
            toPage DataSetDetail DataSetDetailMsg DataSetDetail.update subMsg subModel

        ( ResponseReceived (Ok response), _ ) ->
            { model | lastResponse = Just response } => Ports.prismHighlight ()

        ( ResponseReceived (Err err), _ ) ->
            -- To Do
            { model | lastResponse = Nothing } => Cmd.none

        ( CheckToken time, _ ) ->
            if Jwt.isExpired time model.config.rawToken |> Result.toMaybe |> Maybe.withDefault True then
                let
                    s =
                        Debug.log "Token is expired" time
                in
                model => Cmd.none
                -- TODO: renew the token somehow
            else
                model => Cmd.none

        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            model => Cmd.none

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model => Cmd.none



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        layout =
            Page.layout
    in
    case model.page of
        NotFound ->
            layout Page.Other model NotFound.view

        Blank ->
            -- This is for the very initial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            Html.text ""
                |> layout Page.Other model

        Error subModel ->
            Error.view subModel
                |> layout Page.Other model

        Home subModel ->
            Home.view subModel
                |> layout Page.Home model
                |> Html.map HomeMsg

        DataSets subModel ->
            DataSets.view subModel
                |> layout Page.DataSets model
                |> Html.map DataSetsMsg

        DataSetDetail subModel ->
            DataSetDetail.view subModel
                |> layout Page.DataSetData model
                |> Html.map DataSetDetailMsg

        Imports subModel ->
            Imports.view subModel
                |> layout Page.Imports model
                |> Html.map ImportsMsg

        Sessions subModel ->
            Sessions.view subModel
                |> layout Page.Sessions model
                |> Html.map SessionsMsg

        Models subModel ->
            Models.view subModel
                |> layout Page.Models model
                |> Html.map ModelsMsg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.responseReceived (Response.decodeXhrResponse >> ResponseReceived)
        , Time.every Time.minute CheckToken
        ]



---- PROGRAM ----


initialPage : Page
initialPage =
    Blank


type alias Flags =
    { apiKey : String
    , url : String
    , token : String
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    setRoute (Route.fromLocation location)
        { page = initialPage
        , config =
            { apiKey = Data.Config.decodeApiKey flags.apiKey
            , baseUrl = flags.url
            , token = Data.Config.decodeToken flags.token
            , rawToken = flags.token
            }
        , error = Nothing
        , lastRequest = ""
        , lastResponse = Nothing
        }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
