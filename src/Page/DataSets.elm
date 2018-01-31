module Page.DataSets exposing (Model, Msg, init, update, view)

import Common
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import RemoteData as Remote
import Request.DataSet
import Table exposing (defaultCustomizations)
import Task
import Util exposing ((=>))
import View.Pager as Pager


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetList : Remote.WebData DataSetList
    , tableState : Table.State
    , config : Config
    }


init : Config -> ( Model, Cmd Msg, Cmd Common.Msg )
init config =
    let
        req =
            Request.DataSet.get config 0

        loadDataSetList =
            req
                |> Remote.sendRequest
                |> Cmd.map DataSetListResponse

        recordRequest =
            Common.logRequest req
    in
    ( Model "DataSets" "This is the list of DataSets" [] Remote.Loading (Table.initialSort "dataSetName") config
    , loadDataSetList
    , recordRequest
    )



-- UPDATE --


type Msg
    = DataSetListResponse (Remote.WebData DataSetList)
    | SetTableState Table.State
    | DeleteDataSet DataSet
    | ChangePage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        DeleteDataSet dataSet ->
            model => Cmd.none

        ChangePage pgNum ->
            { model | dataSetList = Remote.Loading }
                => (Request.DataSet.get model.config pgNum
                        |> Remote.sendRequest
                        |> Cmd.map DataSetListResponse
                   )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        gridView =
            case model.dataSetList of
                Remote.Success dsList ->
                    gridSection dsList model.tableState

                _ ->
                    loadingGrid
    in
    div []
        [ h2 [] [ text model.pageTitle ]
        , div [] [ text model.pageBody ]
        , div [ class "row" ]
            [ div [ class "col-lg-9" ]
                [ div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ]
                        [ h3 [] [ text "Datasets" ]
                        ]
                    , div [] gridView
                    ]
                ]
            ]
        ]


loadingGrid : List (Html Msg)
loadingGrid =
    [ div [ class "panel-body" ]
        [ div [ class "table-responsive" ]
            [ span [] [ text "No data found" ]
            ]
        ]
    , div [ class "panel-footer" ]
        []
    ]


gridSection : DataSetList -> Table.State -> List (Html Msg)
gridSection dataSetList tableState =
    [ div [ class "panel-body" ]
        [ div [ class "table-responsive" ]
            [ Table.view config tableState dataSetList.items ]
        ]
    , div [ class "panel-footer" ]
        [ Pager.view dataSetList ChangePage ]
    ]


config : Table.Config DataSet Msg
config =
    Table.customConfig
        { toId = .dataSetName
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" .dataSetName
            , Table.intColumn "Size" .dataSetSize
            , Table.stringColumn "IsTimeSeries" (\a -> a.isTimeSeries |> toString)
            , actionsColumn
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = toTableAttrs
            }
        }


actionsColumn : Table.Column DataSet Msg
actionsColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = dataSetDeleteButton
        , sorter = Table.unsortable
        }


dataSetDeleteButton : DataSet -> Table.HtmlDetails Msg
dataSetDeleteButton dataSet =
    Table.HtmlDetails []
        [ button [ onClick (DeleteDataSet dataSet) ] [ text "Delete" ]
        ]


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ class "table table-striped" ]
