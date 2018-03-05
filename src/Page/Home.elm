module Page.Home exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString, toDataSetName)
import Dict exposing (Dict)
import Feature exposing (Feature(..))
import Html exposing (..)
import Html.Attributes exposing (class)
import Page.DataSets as DataSets exposing (defaultColumns, DataSetColumns)
import Page.Sessions as Sessions exposing (defaultColumns, SessionColumns)
import RemoteData as Remote
import Table
import View.Extra exposing (viewIf)
import View.Grid as Grid
import Data.Config exposing (Config)
import Util exposing ((=>))
import Request.DataSet

---- MODEL ----


type alias Model =
    { pageTitle : String
    , dataSetList : Remote.WebData DataSetList
    , sessionList : Remote.WebData SessionList
    , dataSetTableState : Table.State
    , sessionTableState : Table.State
    , config : Config
    }


init : Config -> (Model, Cmd Msg)
init config =
    (Model
        "API Dashboard"
        Remote.Loading
        Remote.Loading
        (Table.initialSort "dataSetName")
        (Table.initialSort "Name")
        config) => (Request.DataSet.get config 0 5
        |> Remote.sendRequest
        |> Cmd.map DataSetListResponse)



-- UPDATE --


type Msg
    = None
    | SetDataSetTableState Table.State
    | SetSessionTableState Table.State
    | DataSetListResponse (Remote.WebData DataSetList)
    | SessionListResponse (Remote.WebData SessionList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )
        SetDataSetTableState newState ->
            { model | dataSetTableState = newState }
                            => Cmd.none 
        SetSessionTableState newState ->
            { model | dataSessionTableState = newState }
                            => Cmd.none 
        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none
        SessionListResponse resp ->
            { model | sessionList = resp } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pageTitle ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12 col-md-8 col-g-9 col-xl-9" ]
                [ viewRecentPanel "Dataset" (dataSetListView model) ((,) AppRoutes.DataSets (Just AppRoutes.DataSetAdd))
                , viewRecentPanel "Session" (sessionListView model) ((,) AppRoutes.Sessions Nothing)
                , viewRecentPanel "Model" (div [][]) ((,) AppRoutes.Models Nothing)
                ]
            ]
        ]


configDataSetGrid : Dict String String -> (Dict String String -> DataSetColumns Msg) -> Grid.Config DataSet Msg
configDataSetGrid toolTips columns = --is columns parameter used?
    let
        col : DataSetColumns Msg
        col =
            DataSets.defaultColumns toolTips
    in
    Grid.config
        { toId = \a -> a.dataSetName |> dataSetNameToString
        , toMsg = SetDataSetTableState
        , columns =
            [ col.name |> Grid.makeUnsortable
            , col.actions |> Grid.makeUnsortable
            , col.size |> Grid.makeUnsortable
            , col.shape |> Grid.makeUnsortable
            , col.created |> Grid.makeUnsortable
            , col.modified |> Grid.makeUnsortable
            ]
        }

configSessionGrid : Dict String String -> (Dict String String -> SessionColumns Msg) -> Grid.Config DataSet Msg
configSessionGrid toolTips columns =
    let
        col : SessionColumns Msg
        col =
            Sessions.defaultColumns toolTips
    in
    Grid.config
        { toId = \a -> a.dataSetName |> dataSetNameToString
        , toMsg = SetDataSetTableState
        , columns =
            [ col.name |> Grid.makeUnsortable
            , col.actions |> Grid.makeUnsortable
            , col.status |> Grid.makeUnsortable
            , col.dataSource |> Grid.makeUnsortable
            , col.sessionType |> Grid.makeUnsortable
            , col.created |> Grid.makeUnsortable
            ]
        }

dataSetListView : Model -> Html Msg
dataSetListView model =
    Grid.view .items (configDataSetGrid model.config.toolTips DataSets.defaultColumns) model.dataSetTableState model.dataSetList

sessionListView : Model -> Html Msg
sessionListView model =
    Grid.view .items (configSessionGrid model.config.toolTips Sessions.defaultColumns) model.sessionTableState model.sessionList


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
