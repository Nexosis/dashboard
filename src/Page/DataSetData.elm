module Page.DataSetData exposing (..)

import Data.Context exposing (ContextModel, contextToAuth, setPageSize)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Nexosis.Api.Data exposing (getRetrieveDetail)
import Nexosis.Types.Columns exposing (ColumnMetadata)
import Nexosis.Types.DataSet as DataSet exposing (DataSetData, DataSetName, toDataSetName)
import Nexosis.Types.SortParameters exposing (SortDirection(..), SortParameters)
import RemoteData as Remote
import StateStorage
import Util exposing ((=>), formatDisplayName, styledNumber)
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager


type Msg
    = DataLoaded (Remote.WebData DataSetData)
    | ChangePage Int
    | ChangePageSize Int
    | TableChanged Grid.State


type alias Model =
    { dataSetName : DataSetName
    , tableState : Grid.State
    , currentPage : Int
    , pageSize : Int
    , loadingResponse : Remote.WebData DataSetData
    , columns : List ColumnMetadata
    }


loadData : ContextModel -> Model -> Cmd Msg
loadData context model =
    getRetrieveDetail (contextToAuth context) model.dataSetName model.currentPage model.pageSize
        |> Remote.sendRequest
        |> Cmd.map DataLoaded


init : ContextModel -> DataSetName -> ( Model, Cmd Msg )
init context dataSetName =
    let
        model =
            Model dataSetName (Grid.initialSort "" Descending) 0 context.localStorage.userPageSize Remote.Loading []
    in
    model => Cmd.none


dataUpdated : ContextModel -> Model -> Remote.WebData DataSetData -> ( Model, Cmd Msg )
dataUpdated context model resp =
    update (DataLoaded resp) model context


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        DataLoaded resp ->
            let
                columns =
                    case resp of
                        Remote.Success dataSet ->
                            dataSet.columns

                        _ ->
                            model.columns
            in
            { model | loadingResponse = resp, columns = columns } => Cmd.none

        ChangePage pgNum ->
            let
                newCmd =
                    loadData context { model | loadingResponse = Remote.Loading, currentPage = pgNum }
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
                    , StateStorage.saveAppState <| setPageSize context pageSize
                    ]

        TableChanged state ->
            { model | tableState = state } => Cmd.none


view : ContextModel -> Model -> Html Msg
view context model =
    div [ id "viewDataset" ]
        [ div [ class "row mb25" ]
            [ div [ class "col-sm-3 pl0" ] []
            , div [ class "col-sm-6" ]
                [ Pager.view model.loadingResponse ChangePage ]
            , div [ class "col-sm-2 col-sm-offset-1 right" ]
                [ PageSize.view ChangePageSize context.localStorage.userPageSize ]
            ]
        , div [ class "row mb25" ]
            [ div [ class "col-sm-12 p0" ]
                [ div [ class "table-responsive" ] [ viewDataGrid model ]
                , hr [] []
                , div [ class "center" ]
                    [ Pager.view model.loadingResponse ChangePage ]
                ]
            ]
        ]


viewDataGrid : Model -> Html Msg
viewDataGrid model =
    Grid.view .data (config model.columns) model.tableState model.loadingResponse


config : List ColumnMetadata -> Grid.Config (Dict.Dict String String) Msg
config columns =
    Grid.config
        { toId = \a -> ""
        , toMsg = TableChanged
        , columns =
            columns
                |> List.map (\c -> Grid.makeUnsortable (Grid.stringColumn (formatDisplayName c.name) (\r -> Dict.get c.name r |> Maybe.withDefault "")))
        }
