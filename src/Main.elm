module Main exposing (..)

import Html exposing (..)
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Value)
import Page.Error as Error exposing (PageLoadError)
import Page.NotFound as NotFound
import Page.Home as Home
import Page.DataSets as DataSets
import Page.Imports as Imports
import Page.Sessions as Sessions
import Page.Models as Models
import View.Page as Page exposing (ActivePage)
import Route exposing (..)
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { page : Page
    }


type Page
    = Blank
    | NotFound
    | Error PageLoadError
    | Home Home.Model
    | DataSets DataSets.Model
    | Imports Imports.Model
    | Sessions Sessions.Model
    | Models Models.Model



---- UPDATE ----


type Msg
    = SetRoute (Maybe Route)
    | HomeMsg Home.Msg
    | DataSetsMsg DataSets.Msg
    | ImportsMsg Imports.Msg
    | SessionsMsg Sessions.Msg
    | ModelsMsg Models.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute route model =
    case route of
        Nothing ->
            -- TODO Load 404 page not found
            ( model, Cmd.none )

        Just Route.Home ->
            ( { model | page = Home Home.init }, Cmd.none )

        Just Route.DataSets ->
            ( { model | page = DataSets DataSets.init }, Cmd.none )

        Just Route.Imports ->
            ( { model | page = Imports Imports.init }, Cmd.none )

        Just Route.Sessions ->
            ( { model | page = Sessions Sessions.init }, Cmd.none )

        Just Route.Models ->
            ( { model | page = Models Models.init }, Cmd.none )


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
                ( { model | page = (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, page ) of
            -- Update for page transitions
            ( SetRoute route, _ ) ->
                setRoute route model

            -- Update for page specfic msgs
            ( HomeMsg subMsg, Home subModel ) ->
                toPage Home HomeMsg (Home.update) subMsg subModel

            ( DataSetsMsg subMsg, DataSets subModel ) ->
                toPage DataSets DataSetsMsg (DataSets.update) subMsg subModel

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
                layout Page.Other NotFound.view

            Blank ->
                -- This is for the very intial page load, while we are loading
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
    Sub.none



---- PROGRAM ----


initialPage : Page
initialPage =
    Blank


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { page = initialPage
        }


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
