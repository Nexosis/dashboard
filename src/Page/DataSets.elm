module Page.DataSets exposing (DataSetColumns, Model, Msg(DataSetListResponse), init, loadDataSetList, update, view, viewDataSetGridReadonly)

import AppRoutes exposing (Route)
import Data.Cascade as Cascade
import Data.Context exposing (..)
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString, toDataSetName)
import Data.DisplayDate exposing (toShortDateString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import RemoteData as Remote
import Request.DataSet
import Request.Sorting exposing (SortDirection(..), SortParameters)
import StateStorage
import Util exposing ((=>), commaFormatInteger, dataSizeWithSuffix, formatDisplayName, isJust, spinner, styledNumber)
import View.Breadcrumb as Breadcrumb
import View.DeleteDialog as DeleteDialog
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


---- MODEL ----


type alias DataSetColumns msg =
    { name : Grid.Column DataSet msg
    , actions : Grid.Column DataSet msg
    , size : Grid.Column DataSet msg
    , shape : Grid.Column DataSet msg
    , created : Grid.Column DataSet msg
    , modified : Grid.Column DataSet msg
    }


defaultColumns : Dict String String -> DataSetColumns msg
defaultColumns tooltips =
    DataSetColumns nameColumn
        actionsColumn
        sizeColumn
        (shapeColumn tooltips)
        (Grid.customNumberColumn "dateCreated" (\a -> toShortDateString a.dateCreated) [ class "per10" ] [ text "Created" ])
        (Grid.customNumberColumn "lastModified" (\a -> toShortDateString a.lastModified) [ class "per10" ] [ text "Modified" ])


type alias Model =
    { dataSetList : Remote.WebData DataSetList
    , currentPage : Int
    , pageSize : Int
    , tableState : Grid.State
    , deleteDialogModel : Maybe DeleteDialog.Model
    }


loadDataSetList : ContextModel -> Int -> Int -> SortParameters -> Cmd Msg
loadDataSetList context pageNum pageSize sorting =
    Request.DataSet.get context.config pageNum pageSize sorting
        |> Remote.sendRequest
        |> Cmd.map DataSetListResponse


init : ContextModel -> ( Model, Cmd Msg )
init context =
    let
        initialSorting =
            Grid.initialSort "" Ascending
    in
    Model Remote.Loading 0 context.userPageSize initialSorting Nothing
        => loadDataSetList context 0 context.userPageSize initialSorting



-- UPDATE --


type Msg
    = DataSetListResponse (Remote.WebData DataSetList)
    | SetTableState Grid.State
    | ChangePage Int
    | ChangePageSize Int
    | ShowDeleteDialog DataSet
    | DeleteDialogMsg DeleteDialog.Msg


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SetTableState newState ->
            let
                sortedRequest =
                    loadDataSetList context 0 context.userPageSize newState
            in
            { model | tableState = newState, dataSetList = Remote.Loading }
                => sortedRequest

        ChangePage pgNum ->
            { model | dataSetList = Remote.Loading, currentPage = pgNum }
                => loadDataSetList context pgNum context.userPageSize model.tableState

        ChangePageSize pageSize ->
            let
                newModel =
                    { model | pageSize = pageSize, currentPage = 0 }
            in
            newModel
                => Cmd.batch
                    [ loadDataSetList context 0 pageSize model.tableState
                    , StateStorage.saveAppState { context | userPageSize = pageSize }
                    ]

        ShowDeleteDialog dataSet ->
            let
                dsName =
                    dataSetNameToString dataSet.dataSetName
            in
            { model | deleteDialogModel = Just (DeleteDialog.init dsName dsName) } => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                pendingDeleteCmd =
                    toDataSetName >> Request.DataSet.delete context.config

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            loadDataSetList context model.currentPage context.userPageSize model.tableState
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]



-- VIEW --


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ id "page-header", class "row" ]
            [ Breadcrumb.list
            , div [ class "col-sm-6" ]
                [ h2 []
                    ([ text "DataSets " ]
                        ++ helpIcon context.config.toolTips "Datasets"
                    )
                ]
            , div [ class "col-sm-6 right" ]
                [ a [ AppRoutes.href AppRoutes.DataSetAdd, class "btn btn-danger mt10" ] [ i [ class "fa fa-plus mr5" ] [], text "Add dataset" ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "row mb25" ]
                    [ div [ class "col-sm-6 col-sm-offset-3" ]
                        [ Pager.view model.dataSetList ChangePage ]
                    , div [ class "col-sm-2 col-sm-offset-1 right" ]
                        [ PageSize.view ChangePageSize context.userPageSize ]
                    ]
                , viewDataSetGrid context.config.toolTips model.tableState model.dataSetList
                , hr [] []
                , div [ class "center" ]
                    [ Pager.view model.dataSetList ChangePage ]
                ]
            ]
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete DataSet"
            , bodyMessage = Just "This action cannot be undone but you can always upload it again in the future."
            , associatedAssets = [ Cascade.View, Cascade.Session, Cascade.Model, Cascade.Vocabulary ]
            }
            |> Html.map DeleteDialogMsg
        ]


viewDataSetGrid : Dict String String -> Grid.State -> Remote.WebData DataSetList -> Html Msg
viewDataSetGrid toolTips tableState dataSetList =
    Grid.view .items (config toolTips) tableState dataSetList


viewDataSetGridReadonly : Dict String String -> Grid.State -> Remote.WebData DataSetList -> Html Grid.ReadOnlyTableMsg
viewDataSetGridReadonly toolTips tableState dataSetList =
    Grid.view .items (configReadonly toolTips) tableState dataSetList


configReadonly : Dict String String -> Grid.Config DataSet Grid.ReadOnlyTableMsg
configReadonly toolTips =
    let
        col =
            defaultColumns toolTips
    in
    Grid.configCustom
        { toId = \a -> a.dataSetName |> dataSetNameToString
        , toMsg = Grid.Readonly
        , columns =
            [ col.name |> Grid.makeUnsortable
            , col.actions |> Grid.makeUnsortable
            , col.size |> Grid.makeUnsortable
            , col.shape |> Grid.makeUnsortable
            , col.created |> Grid.makeUnsortable
            , col.modified |> Grid.makeUnsortable
            ]
        , customizations = Grid.toFixedTable
        }


config : Dict String String -> Grid.Config DataSet Msg
config toolTips =
    let
        col =
            defaultColumns toolTips
    in
    Grid.remoteConfig
        { toId = \a -> a.dataSetName |> dataSetNameToString
        , toMsg = SetTableState
        , columns =
            [ col.name
            , col.actions
            , col.size
            , col.shape
            , col.created
            , col.modified
            , deleteColumn
            ]
        }


nameColumn : Grid.Column DataSet msg
nameColumn =
    Grid.veryCustomColumn
        { name = "dataSetName"
        , viewData = dataSetNameCell
        , sorter = Grid.increasingOrDecreasingBy (\a -> a.dataSetName |> dataSetNameToString)
        , headAttributes = [ class "left fixed" ]
        , headHtml = [ text "Name" ]
        }


dataSetNameCell : DataSet -> Grid.HtmlDetails msg
dataSetNameCell dataSet =
    Grid.HtmlDetails [ class "left name fixed" ]
        [ a [ AppRoutes.href (AppRoutes.DataSetDetail dataSet.dataSetName) ] [ text (formatDisplayName <| dataSetNameToString dataSet.dataSetName) ] ]


actionsColumn : Grid.Column DataSet msg
actionsColumn =
    Grid.veryCustomColumn
        { name = ""
        , viewData = dataSetActionButton
        , sorter = Grid.unsortable
        , headAttributes = [ class "per15" ]
        , headHtml = []
        }


dataSetActionButton : DataSet -> Grid.HtmlDetails msg
dataSetActionButton dataSet =
    Grid.HtmlDetails [ class "action" ]
        [ a [ AppRoutes.href (AppRoutes.SessionStart dataSet.dataSetName) ] [ button [ class "btn btn-danger btn-sm" ] [ text "Start Session" ] ] ]


deleteColumn : Grid.Column DataSet Msg
deleteColumn =
    Grid.veryCustomColumn
        { name = "Delete"
        , viewData = dataSetDeleteButton
        , sorter = Grid.unsortable
        , headAttributes = [ class "per5" ]
        , headHtml = []
        }


dataSetDeleteButton : DataSet -> Grid.HtmlDetails Msg
dataSetDeleteButton dataSet =
    Grid.HtmlDetails []
        [ a [ onClick (ShowDeleteDialog dataSet), alt "Delete", attribute "role" "button" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]


sizeColumn : Grid.Column DataSet msg
sizeColumn =
    Grid.veryCustomColumn
        { name = "dataSetSize"
        , viewData = sizeCell
        , sorter = Grid.increasingOrDecreasingBy (\a -> toString a.dataSetSize)
        , headAttributes = [ class "per10" ]
        , headHtml = [ text "Size" ]
        }


sizeCell : DataSet -> Grid.HtmlDetails msg
sizeCell dataset =
    Grid.HtmlDetails [ class "number" ] [ text (dataSizeWithSuffix dataset.dataSetSize) ]


shapeColumn : Dict String String -> Grid.Column DataSet msg
shapeColumn tooltips =
    Grid.veryCustomColumn
        { name = "Shape"
        , viewData = shapeCell
        , sorter = Grid.unsortable
        , headAttributes = [ class "per15" ]
        , headHtml = text "Shape" :: helpIcon tooltips "Shape"
        }


shapeCell : DataSet -> Grid.HtmlDetails msg
shapeCell dataset =
    Grid.HtmlDetails [ class "number" ] [ text (commaFormatInteger dataset.rowCount ++ " x " ++ commaFormatInteger dataset.columnCount) ]
