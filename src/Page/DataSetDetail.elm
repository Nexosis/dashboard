module Page.DataSetDetail exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Data.DataSet exposing (ColumnMetadata, ColumnStats, ColumnStatsDict, DataSet, DataSetData, DataSetName, DataSetStats)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode
import RemoteData as Remote
import Request.DataSet
import Request.Log as Log
import Table exposing (defaultCustomizations)
import Util exposing ((=>))
import VegaLite exposing (Spec)
import View.Grid as Grid
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetResponse : Remote.WebData DataSetData
    , columnResponse : Remote.WebData ColumnMetadataListing
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


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    let
        loadDataSetList =
            Request.DataSet.getRetrieveDetail config dataSetName
                |> Remote.sendRequest
                |> Cmd.map DataSetDataResponse
    in
    Model "DataSets" "This is the list of DataSets" [] Remote.Loading Remote.Loading (Table.initialSort "dataSetName") config
        => loadDataSetList


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
                        , stats = Dict.get i.metadata.name stats.columns
                        }
                    )
    in
    { metadataListing | metadataList = updatedListing }



-- UPDATE --


type Msg
    = DataSetDataResponse (Remote.WebData DataSetData)
    | StatsResponse (Remote.WebData DataSetStats)
    | SetTableState Table.State
    | DeleteDataSet DataSet
    | ChangePage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataSetDataResponse resp ->
            case resp of
                Remote.Success dataSetDetail ->
                    let
                        vegaSpec =
                            dataSetDetail.columns
                                |> List.map generateVegaSpec
                                |> Json.Encode.object

                        columnListing =
                            mapColumnListToPagedListing dataSetDetail.columns

                        statsRequest =
                            Request.DataSet.getStats model.config dataSetDetail.dataSetName
                                |> Remote.sendRequest
                                |> Cmd.map StatsResponse
                    in
                    -- Send ports when we want to draw the histogram.
                    -- Ports.drawVegaChart vegaSpec
                    { model | dataSetResponse = resp, columnResponse = Remote.succeed columnListing } => statsRequest

                _ ->
                    { model | dataSetResponse = resp } => Cmd.none

        StatsResponse resp ->
            case resp of
                Remote.Success _ ->
                    let
                        updatedColumnInfo =
                            Remote.map2 mergeListingAndStats model.columnResponse resp
                    in
                    { model | columnResponse = updatedColumnInfo } => Cmd.none

                Remote.Failure err ->
                    model => (Log.logMessage <| Log.LogMessage ("Stat response failure: " ++ toString err) Log.Error)

                _ ->
                    model => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        DeleteDataSet dataSet ->
            model => Cmd.none

        ChangePage pageNumber ->
            let
                ( columnListing, cmd ) =
                    Remote.update (updateColumnPageNumber pageNumber) model.columnResponse
            in
            { model | columnResponse = columnListing } => cmd


generateVegaSpec : ColumnMetadata -> ( String, Spec )
generateVegaSpec column =
    column.name
        => VegaLite.toVegaLite
            [ VegaLite.title column.name
            , VegaLite.dataFromColumns [] <| VegaLite.dataColumn "x" (VegaLite.Numbers [ 10, 20, 30 ]) []
            , VegaLite.mark VegaLite.Circle []
            , VegaLite.encoding <| VegaLite.position VegaLite.X [ VegaLite.PName "x", VegaLite.PmType VegaLite.Quantitative ] []
            ]


updateColumnPageNumber : Int -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd msg )
updateColumnPageNumber pageNumber columnListing =
    { columnListing | pageNumber = pageNumber } => Cmd.none


updateColumnMetadata : List ColumnInfo -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd msg )
updateColumnMetadata info columnListing =
    { columnListing | metadataList = info } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        --todo breadcrumb
        [ p [ class "breadcrumb" ]
            [ span []
                [ a [ href "#" ] [ text "API Dashboard" ]
                , i [ class "fa fa-angle-right", style [ ( "margin", "0 5px" ) ] ] []
                , a [ href "#" ] [ text "Datasets" ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-6" ]
                --todo - fill name
                [ h2 [ class "mt10" ] [ text "{DatasetName}" ] ]
            , div [ class "col-sm-6 right" ]
                [ a [ href "#", class "btn mt10" ] [ text "Start Session" ]
                ]
            , div [ class "row" ]
                [ div [ class "col-sm-8" ]
                    [ p [ class "small" ]
                        [ strong [] [ text "Dataset ID:" ]

                        --todo - DS id here
                        , text "1234"

                        -- todo - Copy id
                        , a [ href "#" ] [ i [ class "fa fa-copy color-mediumGray" ] [] ]
                        ]
                    ]
                , div [ class "col-sm-4 right" ]
                    [ button [ class "btn btn-xs secondary" ] [ i [ class "fa fa-trash-o mr5" ] [], text " Delete" ]
                    ]
                ]
            , hr [] []
            , div [ class "row" ]
                [-- ds details here
                ]
            , hr [] []
            , div [ class "row" ]
                [ div [ class "col-sm-12" ]
                    [ div [ class "row mb25" ]
                        [ div [ class "col-sm-3" ]
                            [ h3 [] [ text "Columns" ]
                            ]
                        , div [ class "col-sm-2 col-sm-offset-7 right" ]
                            [--todo : page number changer
                            ]
                        ]
                    , Grid.view filterColumnsToDisplay (config model.config.toolTips) model.tableState model.columnResponse
                    , div [ class "center" ] [ Pager.view model.columnResponse ChangePage ]
                    ]
                ]
            ]
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
            [ option [] [ text column.metadata.dataType ]
            ]
        ]


roleColumn : (String -> List (Html Msg)) -> Grid.Column ColumnInfo Msg
roleColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Role"
        , viewData = dataTypeCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Role"
        }


roleCell : ColumnInfo -> Table.HtmlDetails Msg
roleCell column =
    Table.HtmlDetails [ class "form-group" ]
        [ select [ class "form-control" ]
            [ option [] [ text column.metadata.role ]
            ]
        ]


imputationColumn : (String -> List (Html Msg)) -> Grid.Column ColumnInfo Msg
imputationColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Imputation"
        , viewData = dataTypeCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Imputation"
        }


imputationCell : ColumnInfo -> Table.HtmlDetails Msg
imputationCell column =
    Table.HtmlDetails [ class "form-group" ]
        [ select [ class "form-control" ]
            [ option [] [ text column.metadata.imputation ]
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
    columnStats
        |> Maybe.map (\s -> div [] [ text (toString s.max) ])
        |> Maybe.withDefault
            (div
                []
                [ text "-" ]
            )


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
