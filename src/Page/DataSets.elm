module Page.DataSets exposing (DataSetColumns, Model, Msg(DataSetListResponse), init, loadDataSetList, update, view, viewDataSetGridReadonly)

import AppRoutes exposing (Route)
import Data.Cascade as Cascade
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString, toDataSetName)
import Data.DisplayDate exposing (toShortDateString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import RemoteData as Remote
import Request.DataSet
import Table exposing (defaultCustomizations)
import Util exposing ((=>), commaFormatInteger, dataSizeWithSuffix, isJust, spinner, styledNumber)
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
        (Grid.customStringColumn "Created" (\a -> toShortDateString a.dateCreated) [ class "per10" ] [])
        (Grid.customStringColumn "Modified" (\a -> toShortDateString a.lastModified) [ class "per10" ] [])


type alias Model =
    { dataSetList : Remote.WebData DataSetList
    , currentPage : Int
    , pageSize : Int
    , tableState : Table.State
    , config : Config
    , deleteDialogModel : Maybe DeleteDialog.Model
    }


loadDataSetList : Config -> Int -> Int -> Cmd Msg
loadDataSetList config pageNum pageSize =
    Request.DataSet.get config pageNum pageSize
        |> Remote.sendRequest
        |> Cmd.map DataSetListResponse


init : Config -> ( Model, Cmd Msg )
init config =
    Model Remote.Loading 0 10 (Table.initialSort "dataSetName") config Nothing
        => loadDataSetList config 0 10



-- UPDATE --


type Msg
    = DataSetListResponse (Remote.WebData DataSetList)
    | SetTableState Table.State
    | ChangePage Int
    | ChangePageSize Int
    | ShowDeleteDialog DataSet
    | DeleteDialogMsg DeleteDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        ChangePage pgNum ->
            { model | dataSetList = Remote.Loading, currentPage = pgNum }
                => loadDataSetList model.config pgNum model.pageSize

        ChangePageSize pageSize ->
            { model | pageSize = pageSize, currentPage = 0 }
                => loadDataSetList model.config 0 pageSize

        ShowDeleteDialog dataSet ->
            let
                dsName =
                    dataSetNameToString dataSet.dataSetName
            in
            { model | deleteDialogModel = Just (DeleteDialog.init dsName dsName) } => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                pendingDeleteCmd =
                    toDataSetName >> Request.DataSet.delete model.config

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            loadDataSetList model.config model.currentPage model.pageSize
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        --todo - breadcrumbs ?
        [ p [ class "breadcrumb" ] [ span [] [ a [ href "#" ] [ text "API Dashboard" ] ] ]
        , div [ class "row" ]
            [ div [ class "col-sm-6" ]
                [ h2 [ class "mt10" ] [ text "Datasets" ]
                ]
            , div [ class "col-sm-6 right" ]
                [ a [ AppRoutes.href AppRoutes.DataSetAdd, class "btn mt10" ] [ i [ class "fa fa-plus mr5" ] [], text "Add DataSet" ]
                ]
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "row mb25" ]
                    [ div [ class "col-sm-6" ]
                        [ h3 [] [ text "Dataset explainer" ]
                        ]
                    , div [ class "col-sm-2 col-sm-offset-4 right" ]
                        [ PageSize.view ChangePageSize ]
                    ]
                , viewDataSetGrid model.config.toolTips model.tableState model.dataSetList
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


viewDataSetGrid : Dict String String -> Table.State -> Remote.WebData DataSetList -> Html Msg
viewDataSetGrid toolTips tableState dataSetList =
    Grid.view .items (config toolTips) tableState dataSetList


viewDataSetGridReadonly : Dict String String -> Table.State -> Remote.WebData DataSetList -> Html Grid.ReadOnlyTableMsg
viewDataSetGridReadonly toolTips tableState dataSetList =
    Grid.view .items (configReadonly toolTips) tableState dataSetList


configReadonly : Dict String String -> Grid.Config DataSet Grid.ReadOnlyTableMsg
configReadonly toolTips =
    let
        col =
            defaultColumns toolTips
    in
    Grid.config
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
        }


config : Dict String String -> Grid.Config DataSet Msg
config toolTips =
    let
        col =
            defaultColumns toolTips
    in
    Grid.config
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
        { name = "Name"
        , viewData = dataSetNameCell
        , sorter = Table.increasingOrDecreasingBy (\a -> a.dataSetName |> dataSetNameToString)
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


dataSetNameCell : DataSet -> Table.HtmlDetails msg
dataSetNameCell dataSet =
    Table.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.DataSetDetail dataSet.dataSetName) ] [ text (dataSetNameToString dataSet.dataSetName) ] ]


actionsColumn : Grid.Column DataSet msg
actionsColumn =
    Grid.veryCustomColumn
        { name = ""
        , viewData = dataSetActionButton
        , sorter = Table.unsortable
        , headAttributes = []
        , headHtml = []
        }


dataSetActionButton : DataSet -> Table.HtmlDetails msg
dataSetActionButton dataSet =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        --todo - Change the button text and color based on the status
        [ a [ class "btn btn-sm", AppRoutes.href (AppRoutes.SessionStart dataSet.dataSetName) ] [ text "Start Session" ] ]


deleteColumn : Grid.Column DataSet Msg
deleteColumn =
    Grid.veryCustomColumn
        { name = "Delete"
        , viewData = dataSetDeleteButton
        , sorter = Table.unsortable
        , headAttributes = [ class "per5" ]
        , headHtml = []
        }


dataSetDeleteButton : DataSet -> Table.HtmlDetails Msg
dataSetDeleteButton dataSet =
    Table.HtmlDetails []
        [ button [ onClick (ShowDeleteDialog dataSet), alt "Delete", class "btn-link" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]


sizeColumn : Grid.Column DataSet msg
sizeColumn =
    Grid.veryCustomColumn
        { name = "Size"
        , viewData = sizeCell
        , sorter = Table.increasingOrDecreasingBy (\a -> toString a.dataSetSize)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


sizeCell : DataSet -> Table.HtmlDetails msg
sizeCell dataset =
    Table.HtmlDetails [] [ styledNumber (dataSizeWithSuffix dataset.dataSetSize) ]


shapeColumn : Dict String String -> Grid.Column DataSet msg
shapeColumn tooltips =
    Grid.veryCustomColumn
        { name = "Shape"
        , viewData = shapeCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per15" ]
        , headHtml = helpIcon tooltips "Shape"
        }


shapeCell : DataSet -> Table.HtmlDetails msg
shapeCell dataset =
    Table.HtmlDetails [] [ styledNumber (commaFormatInteger dataset.rowCount ++ " x " ++ commaFormatInteger dataset.columnCount) ]
