module View.ColumnMetadataEditor exposing (Model, Msg, init, update, view)

import Data.Columns as Columns exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.DataSet as DataSet exposing (ColumnStats, ColumnStatsDict, DataSetData, DataSetName, DataSetStats, dataSetNameToString, toDataSetName)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData as Remote
import Request.DataSet
import Request.Log exposing (logHttpError)
import Table
import Util exposing ((=>))
import VegaLite exposing (Spec)
import View.Grid as Grid
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


type alias Model =
    { columnMetadata : Remote.WebData ColumnMetadataListing
    , dataSetName : DataSetName
    , tableState : Table.State
    , config : Config
    }


type alias ColumnMetadataListing =
    { pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    , metadataList : List ColumnInfo
    }


type alias ColumnInfo =
    { metadata : ColumnMetadata
    , stats : Maybe ColumnStats
    }


type Msg
    = StatsResponse (Remote.WebData DataSetStats)
    | SetTableState Table.State
    | ChangePage Int


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    let
        statsRequest =
            Request.DataSet.getStats config dataSetName
                |> Remote.sendRequest
                |> Cmd.map StatsResponse
    in
    Model Remote.Loading dataSetName (Table.initialSort "columnName") config
        => statsRequest


mapColumnListToPagedListing : List ColumnMetadata -> ColumnMetadataListing
mapColumnListToPagedListing columns =
    let
        count =
            List.length columns

        pageSize =
            10
    in
    { pageNumber = 0
    , totalPages = count // 10
    , pageSize = pageSize
    , totalCount = count
    , metadataList =
        List.map
            (\m ->
                { metadata = m
                , stats = Nothing
                }
            )
            columns
    }


mergeListingAndStats : ColumnMetadataListing -> DataSetStats -> ColumnMetadataListing
mergeListingAndStats metadataListing stats =
    let
        updatedListing =
            metadataListing.metadataList
                |> List.map
                    (\i ->
                        { metadata = i.metadata
                        , stats = Dict.get (String.toLower i.metadata.name) stats.columns
                        }
                    )
    in
    { metadataListing | metadataList = updatedListing }


update : Msg -> Model -> Remote.WebData DataSetData -> ( Model, Cmd Msg )
update msg oldModel dataSetResponse =
    let
        model =
            if not <| Remote.isLoading oldModel.columnMetadata then
                let
                    mappedColumns =
                        Remote.map (.columns >> mapColumnListToPagedListing) dataSetResponse
                in
                { oldModel | columnMetadata = mappedColumns }
            else
                oldModel
    in
    case msg of
        StatsResponse resp ->
            case resp of
                Remote.Success _ ->
                    let
                        updatedColumnInfo =
                            Remote.map2 mergeListingAndStats model.columnMetadata resp
                    in
                    { model | columnMetadata = updatedColumnInfo } => Cmd.none

                Remote.Failure err ->
                    model => logHttpError err

                _ ->
                    model => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        ChangePage pageNumber ->
            let
                ( columnListing, cmd ) =
                    Remote.update (updateColumnPageNumber pageNumber) model.columnMetadata
            in
            { model | columnMetadata = columnListing } => Cmd.none


updateColumnPageNumber : Int -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd msg )
updateColumnPageNumber pageNumber columnListing =
    { columnListing | pageNumber = pageNumber } => Cmd.none


updateColumnMetadata : List ColumnInfo -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd msg )
updateColumnMetadata info columnListing =
    { columnListing | metadataList = info } => Cmd.none


generateVegaSpec : ColumnMetadata -> ( String, Spec )
generateVegaSpec column =
    column.name
        => VegaLite.toVegaLite
            [ VegaLite.title column.name
            , VegaLite.dataFromColumns [] <| VegaLite.dataColumn "x" (VegaLite.Numbers [ 10, 20, 30 ]) []
            , VegaLite.mark VegaLite.Circle []
            , VegaLite.encoding <| VegaLite.position VegaLite.X [ VegaLite.PName "x", VegaLite.PmType VegaLite.Quantitative ] []
            ]


view : Model -> Html Msg
view model =
    div [ class "row mb25" ]
        [ div [ class "col-sm-3" ]
            [ h3 [] [ text "Columns" ]
            ]
        , div [ class "col-sm-2 col-sm-offset-7 right" ]
            [--todo : page number changer
            ]
        , Grid.view filterColumnsToDisplay (config model.config.toolTips) model.tableState model.columnMetadata
        , div [ class "center" ] [ Pager.view model.columnMetadata ChangePage ]
        ]


filterColumnsToDisplay : ColumnMetadataListing -> List ColumnInfo
filterColumnsToDisplay columnListing =
    let
        drop =
            columnListing.pageSize * columnListing.pageNumber
    in
    columnListing.metadataList
        |> List.drop drop
        |> List.take columnListing.pageSize


config : Dict String String -> Grid.Config ColumnInfo Msg
config toolTips =
    let
        makeIcon =
            helpIcon toolTips
    in
    Grid.config
        { toId = \c -> c.metadata.name
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , typeColumn makeIcon
            , roleColumn makeIcon
            , imputationColumn makeIcon
            , statsColumn
            ]
        }


nameColumn : Grid.Column ColumnInfo Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Column Name"
        , viewData = columnNameCell
        , sorter = Table.increasingOrDecreasingBy (\c -> c.metadata.name)
        , headAttributes = [ class "left per25" ]
        , headHtml = []
        }


columnNameCell : ColumnInfo -> Table.HtmlDetails Msg
columnNameCell column =
    Table.HtmlDetails [ class "name" ]
        [ text column.metadata.name ]


typeColumn : (String -> List (Html Msg)) -> Grid.Column ColumnInfo Msg
typeColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Type"
        , viewData = dataTypeCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Type"
        }


dataTypeCell : ColumnInfo -> Table.HtmlDetails Msg
dataTypeCell column =
    Table.HtmlDetails [ class "form-group" ]
        [ select [ class "form-control" ]
            [ option [] [ text <| toString column.metadata.dataType ]
            ]
        ]


roleColumn : (String -> List (Html Msg)) -> Grid.Column ColumnInfo Msg
roleColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Role"
        , viewData = roleCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Role"
        }


roleCell : ColumnInfo -> Table.HtmlDetails Msg
roleCell column =
    Table.HtmlDetails [ class "form-group" ]
        [ select [ class "form-control" ]
            [ option [] [ text <| toString column.metadata.role ]
            ]
        ]


imputationColumn : (String -> List (Html Msg)) -> Grid.Column ColumnInfo Msg
imputationColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Imputation"
        , viewData = imputationCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Imputation"
        }


imputationCell : ColumnInfo -> Table.HtmlDetails Msg
imputationCell column =
    Table.HtmlDetails [ class "form-group" ]
        [ select [ class "form-control" ]
            [ option [] [ text <| toString column.metadata.imputation ]
            ]
        ]


statsColumn : Grid.Column ColumnInfo Msg
statsColumn =
    Grid.veryCustomColumn
        { name = "Stats"
        , viewData = statsCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per20", colspan 2 ]
        , headHtml = []
        }


statsCell : ColumnInfo -> Table.HtmlDetails Msg
statsCell column =
    Table.HtmlDetails [ class "stats" ]
        [ statsDisplay column.stats ]


statsDisplay : Maybe ColumnStats -> Html Msg
statsDisplay columnStats =
    case columnStats of
        Just stats ->
            div [ class "row m0" ]
                [ div [ class "col-sm-6 pl0 pr0" ]
                    [ strong [] [ text "Min: " ]
                    , text <| toString stats.min
                    , br [] []
                    , strong [] [ text "Max: " ]
                    , text <| toString stats.max
                    , br [] []
                    , strong [] [ text "Standard Deviation: " ]
                    , text <| toString stats.stddev
                    , br [] []
                    , strong [ class "text-danger" ] [ text "Errors: " ]
                    , text <| toString stats.errors
                    ]
                , div [ class "col-sm-6 pl0 pr0" ]
                    [ strong [] [ text "Value Count: " ]
                    , text <| toString stats.row_count
                    , br [] []
                    , strong [ class "text-danger" ] [ text "# Missing: " ]
                    , text <| toString stats.missing
                    , br [] []
                    , strong [] [ text "Mean: " ]
                    , text <| toString stats.mean
                    , br [] []
                    , strong [] [ text "Median: " ]
                    , text <| toString stats.median
                    ]
                ]

        Nothing ->
            div [ class "row m0" ]
                [ div [ class "col-sm-6 pl0 pr0" ] []
                , div [ class "col-sm-6 pl0 pr0" ] []
                ]


histogramColumn : Grid.Column ColumnInfo Msg
histogramColumn =
    Grid.veryCustomColumn
        { name = "Distribution"
        , viewData = histogram
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


histogram : ColumnInfo -> Table.HtmlDetails Msg
histogram column =
    Table.HtmlDetails []
        [ div [ id ("histogram_" ++ column.metadata.name) ] [] ]
