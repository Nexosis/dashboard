module Page.Models exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Config exposing (Config)
import Data.Model exposing (ModelData, ModelList)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData as Remote
import Request.Model exposing (get)
import Table
import Util exposing ((=>))
import View.Grid as Grid


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , modelList : Remote.WebData ModelList
    , tableState : Table.State
    , config : Config
    }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        req =
            Request.Model.get config 0

        loadModelList =
            req
                |> Remote.sendRequest
                |> Cmd.map ModelListResponse
    in
    Model "Models" "This is the list of Models" [] Remote.Loading (Table.initialSort "createdDate") config
        => loadModelList



-- UPDATE --


type Msg
    = ModelListResponse (Remote.WebData ModelList)
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelListResponse resp ->
            { model | modelList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pageTitle ]
        , div [] [ text model.pageBody ]
        , Grid.view .items (config model.config.toolTips) model.tableState model.modelList
        ]


config : Dict String String -> Grid.Config ModelData Msg
config toolTips =
    Grid.config
        { toId = \a -> a.modelId
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            ]
        }


nameColumn : Grid.Column ModelData Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Name"
        , viewData = modelIdCell
        , sorter = Table.increasingOrDecreasingBy (\a -> a.modelId)
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


modelIdCell : ModelData -> Table.HtmlDetails Msg
modelIdCell model =
    Table.HtmlDetails [ class "left name" ]
        [ a [] [ text model.modelId ] ]
