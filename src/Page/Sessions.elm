module Page.Sessions exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Data.Session exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData as Remote
import Request.Session exposing (get)
import Table
import Util exposing ((=>))
import View.Grid as Grid


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , sessionList : Remote.WebData SessionList
    , tableState : Table.State
    , config : Config
    }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        req =
            Request.Session.get config 0

        loadSessionList =
            req
                |> Remote.sendRequest
                |> Cmd.map SessionListResponse
    in
    Model "Sessions" "This is the list of Sessions" Remote.Loading (Table.initialSort "name") config
        => loadSessionList



-- UPDATE --


type Msg
    = SessionListResponse (Remote.WebData SessionList)
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionListResponse resp ->
            { model | sessionList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pageTitle ]
        , div [] [ text model.pageBody ]
        , Grid.view .items (config model.config.toolTips) model.tableState model.sessionList
        ]


config : Dict String String -> Grid.Config SessionData Msg
config toolTips =
    Grid.config
        { toId = \a -> a.sessionId
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            ]
        }


nameColumn : Grid.Column SessionData Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Name"
        , viewData = sessionNameCell
        , sorter = Table.increasingOrDecreasingBy (\a -> a.name)
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


sessionNameCell : SessionData -> Table.HtmlDetails Msg
sessionNameCell model =
    Table.HtmlDetails [ class "left name" ]
        [ a [] [ text model.name ] ]
