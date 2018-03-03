module Page.Home exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString, toDataSetName)
import Dict exposing (Dict)
import Feature exposing (Feature(..))
import Html exposing (..)
import Html.Attributes exposing (class)
import Page.DataSets as DataSets exposing (defaultColumns, DataSetColumns)
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
    , dataSetTableState : Table.State
    , config : Config
    }


init : Config -> (Model, Cmd Msg)
init config =
    (Model
        "API Dashboard"
        Remote.Loading
        (Table.initialSort "dataSetName")
        config) => (Request.DataSet.get config 0 5
        |> Remote.sendRequest
        |> Cmd.map DataSetListResponse)



-- UPDATE --


type Msg
    = None
    | SetDataSetTableState Table.State
    | DataSetListResponse (Remote.WebData DataSetList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )
        SetDataSetTableState newState ->
            { model | dataSetTableState = newState }
                            => Cmd.none 
        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pageTitle ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12 col-md-8 col-g-9 col-xl-9" ]
                [ viewRecentPanel "Dataset" (dataSetListView model) ((,) AppRoutes.DataSets (Just AppRoutes.DataSetAdd))
                , viewRecentPanel "Session" (div [][]) ((,) AppRoutes.Sessions Nothing)
                , viewRecentPanel "Model" (div [][]) ((,) AppRoutes.Models Nothing)
                ]
            ]
        ]


configDataSetGrid : Dict String String -> (Dict String String -> DataSetColumns Msg) -> Grid.Config DataSet Msg
configDataSetGrid toolTips columns =
    let
        col : DataSetColumns Msg
        col =
            defaultColumns toolTips
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

dataSetListView : Model -> Html Msg
dataSetListView model =
    Grid.view .items (configDataSetGrid model.config.toolTips DataSets.defaultColumns) model.dataSetTableState model.dataSetList


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
