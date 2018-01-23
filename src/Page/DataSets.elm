module Page.DataSets exposing (view, update, Model, Msg, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Table exposing (defaultCustomizations)
import RemoteData as Remote
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSetList, DataSet)
import Request.DataSet
import Http
import Task
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetList : Remote.WebData DataSetList
    , tableState : Table.State
    }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        loadDataSetList =
            Request.DataSet.get config
                |> Remote.sendRequest
                |> Cmd.map DataSetListResponse
    in
        (Model "DataSets" "This is the list of DataSets" [] Remote.Loading (Table.initialSort "dataSetName"))
            => loadDataSetList



-- UPDATE --


type Msg
    = Todo
    | DataSetListResponse (Remote.WebData DataSetList)
    | SetTableState Table.State
    | DeleteDataSet DataSet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            model => Cmd.none

        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        DeleteDataSet dataSet ->
            model => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    let
        filteredDataSets =
            case model.dataSetList of
                Remote.Success dsList ->
                    dsList.items

                _ ->
                    []
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
                        , div [ class "panel-body" ]
                            [ div [ class "table-responsive" ]
                                [ Table.view config model.tableState filteredDataSets ]
                            ]
                        , div [ class "panel-footer" ]
                            []
                        ]
                    ]
                ]
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
