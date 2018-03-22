module Page.DataSetData exposing (..)

import Data.Columns exposing (ColumnMetadata)
import Data.Context exposing (ContextModel)
import Data.DataSet as DataSet exposing (DataSetData, DataSetName, toDataSetName)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import RemoteData as Remote
import Request.DataSet
import StateStorage
import Table
import Util exposing ((=>), styledNumber)
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


type Msg
    = DataLoaded (Remote.WebData DataSetData)
    | ChangePage Int
    | ChangePageSize Int
    | TableChanged Table.State


type alias Model =
    { dataSetName : DataSetName
    , tableState : Table.State
    , currentPage : Int
    , pageSize : Int
    , loadingResponse : Remote.WebData DataSetData
    }


loadData : ContextModel -> Model -> Cmd Msg
loadData context model =
    Request.DataSet.getRetrieveDetail context.config model.dataSetName model.currentPage model.pageSize
        |> Remote.sendRequest
        |> Cmd.map DataLoaded


init : ContextModel -> DataSetName -> ( Model, Cmd Msg )
init context dataSetName =
    Model dataSetName (Table.initialSort "createdDate") 0 context.userPageSize Remote.NotAsked => Cmd.none


dataUpdated : ContextModel -> Model -> Remote.WebData DataSetData -> ( Model, Cmd Msg )
dataUpdated context model resp =
    update (DataLoaded resp) model context


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    let
        x =
            Debug.log "Update DataSetData" msg
    in
    case msg of
        DataLoaded resp ->
            { model | loadingResponse = resp } => Cmd.none

        ChangePage pgNum ->
            let
                newCmd =
                    loadData context { model | loadingResponse = Remote.Loading, currentPage = pgNum }

                y =
                    Debug.log "Changing page" newCmd
            in
            { model | loadingResponse = Remote.Loading, currentPage = pgNum } => newCmd

        ChangePageSize pageSize ->
            let
                newModel =
                    { model | pageSize = pageSize, currentPage = 0 }
            in
            newModel
                => Cmd.batch
                    [ loadData context newModel
                    , StateStorage.saveAppState { context | userPageSize = pageSize }
                    ]

        TableChanged state ->
            { model | tableState = state } => Cmd.none


view : ContextModel -> Model -> Html Msg
view context model =
    let
        x =
            Debug.log "View Model" model
    in
    div [ class "row mb25" ]
        [ div [ class "col-sm-6" ]
            [ Pager.view model.loadingResponse ChangePage ]
        , div [ class "col-sm-2 col-sm-offset-1 right" ]
            [ PageSize.view ChangePageSize context.userPageSize ]
        , div []
            [ viewDataGrid model
            , hr [] []
            , div [ class "center" ]
                [ Pager.view model.loadingResponse ChangePage ]
            ]
        ]


viewDataGrid : Model -> Html Msg
viewDataGrid model =
    case model.loadingResponse of
        Remote.Success dataSet ->
            Grid.view .columns (config dataSet) model.tableState model.loadingResponse

        _ ->
            text ""


config : DataSetData -> Grid.Config ColumnMetadata Msg
config model =
    Grid.config
        { toId = \a -> a.name
        , toMsg = TableChanged
        , columns =
            List.map dynamicColumn model.columns
        }


dynamicColumn : ColumnMetadata -> Grid.Column ColumnMetadata msg
dynamicColumn column =
    Grid.veryCustomColumn
        { name = column.name
        , viewData = dynamicCell
        , sorter = Table.decreasingOrIncreasingBy (\a -> a.name)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


dynamicCell : ColumnMetadata -> Table.HtmlDetails msg
dynamicCell column =
    Table.HtmlDetails [ class "number" ]
        [ styledNumber column.name
        ]
