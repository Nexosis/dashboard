module Page.DataSetDetail exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Cascade as Cascade
import Data.Columns as Columns exposing (ColumnMetadata, Role)
import Data.Config exposing (Config)
import Data.DataSet as DataSet exposing (ColumnStats, ColumnStatsDict, DataSet, DataSetData, DataSetName, DataSetStats, dataSetNameToString, toDataSetName)
import Data.Link exposing (Link, linkDecoder)
import Data.Session exposing (SessionData, SessionList)
import Dict exposing (Dict)
import Dict.Extra as DictX
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode
import List.Extra as ListX
import RemoteData as Remote
import Request.DataSet
import Request.Log as Log
import Request.Session exposing (getForDataset)
import Table exposing (defaultCustomizations)
import Util exposing ((=>))
import VegaLite exposing (Spec)
import View.DeleteDialog as DeleteDialog
import View.Grid as Grid
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetName : DataSetName
    , dataSetResponse : Remote.WebData DataSetData
    , columnResponse : Remote.WebData ColumnMetadataListing
    , tableState : Table.State
    , config : Config
    , deleteDialogModel : Maybe DeleteDialog.Model
    , sessionLinks : List Link
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
        loadData =
            Cmd.batch
                [ Request.DataSet.getRetrieveDetail config dataSetName
                    |> Remote.sendRequest
                    |> Cmd.map DataSetDataResponse
                , loadRelatedSessions config dataSetName
                ]
    in
    Model "DataSets" "This is the list of DataSets" [] dataSetName Remote.Loading Remote.Loading (Table.initialSort "dataSetName") config Nothing []
        => loadData


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
        -- Keys are currently lower case, but that could change in the future to match the column metadata exactly.
        loweredKeys =
            DictX.mapKeys String.toLower stats.columns

        updatedListing =
            metadataListing.metadataList
                |> List.map
                    (\i ->
                        { metadata = i.metadata
                        , stats = Dict.get (String.toLower i.metadata.name) loweredKeys
                        }
                    )
    in
    { metadataListing | metadataList = updatedListing }


loadRelatedSessions : Config -> DataSetName -> Cmd Msg
loadRelatedSessions config dataset =
    getForDataset config dataset
        |> Remote.sendRequest
        |> Cmd.map SessionDataListResponse



-- UPDATE --


type Msg
    = DataSetDataResponse (Remote.WebData DataSetData)
    | StatsResponse (Remote.WebData DataSetStats)
    | SetTableState Table.State
    | ChangePage Int
    | ShowDeleteDialog
    | DeleteDialogMsg DeleteDialog.Msg
    | SessionDataListResponse (Remote.WebData SessionList)


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

        ChangePage pageNumber ->
            let
                ( columnListing, cmd ) =
                    Remote.update (updateColumnPageNumber pageNumber) model.columnResponse
            in
            { model | columnResponse = columnListing } => cmd

        ShowDeleteDialog ->
            let
                dsName =
                    dataSetNameToString model.dataSetName
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
                            AppRoutes.modifyUrl AppRoutes.DataSets
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]

        SessionDataListResponse listResp ->
            case listResp of
                Remote.Success sessionList ->
                    let
                        subList =
                            List.map (\s -> s.links) sessionList.items
                                |> List.concat
                                |> List.filter (\l -> l.rel == "self")
                    in
                    { model | sessionLinks = subList } => Cmd.none

                Remote.Failure err ->
                    model => (Log.logMessage <| Log.LogMessage ("Stat response failure: " ++ toString err) Log.Error)

                _ ->
                    model => Cmd.none


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
        , viewNameRow model
        , viewIdRow model
        , hr [] []
        , viewDetailsRow model
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
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete DataSet"
            , bodyMessage = Just "This action cannot be undone but you can always upload it again in the future."
            , associatedAssets = [ Cascade.View, Cascade.Session, Cascade.Model, Cascade.Vocabulary ]
            }
            |> Html.map DeleteDialogMsg
        ]


viewNameRow : Model -> Html Msg
viewNameRow model =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ h2 [ class "mt10" ] [ text (DataSet.dataSetNameToString model.dataSetName) ] ]
        , div [ class "col-sm-6 right" ]
            -- link to the start session route when it exists
            [ a [ href "#", class "btn mt10" ] [ text "Start Session" ]
            ]
        ]


viewIdRow : Model -> Html Msg
viewIdRow model =
    div [ class "row" ]
        [ div [ class "col-sm-8" ] []
        , div [ class "col-sm-4 right" ]
            [ button [ class "btn btn-xs secondary", onClick ShowDeleteDialog ] [ i [ class "fa fa-trash-o mr5" ] [], text " Delete" ]
            ]
        ]


viewDetailsRow : Model -> Html Msg
viewDetailsRow model =
    div [ class "row" ]
        [ viewRolesCol model
        , viewDetailsCol model
        , viewRelatedCol model
        ]


viewRolesCol : Model -> Html Msg
viewRolesCol model =
    let
        ( keyFormGroup, targetFormGroup ) =
            case model.columnResponse of
                Remote.Success resp ->
                    let
                        keyGroup =
                            resp.metadataList
                                |> ListX.find (\m -> m.metadata.role == Columns.Key)
                                |> Maybe.map viewKeyFormGroup
                                |> Maybe.withDefault (div [] [])

                        targetGroup =
                            resp.metadataList
                                |> ListX.find (\m -> m.metadata.role == Columns.Target)
                                |> viewTargetFormGroup
                    in
                    ( keyGroup, targetGroup )

                Remote.Loading ->
                    ( viewLoadingFormGroup, viewLoadingFormGroup )

                _ ->
                    ( div [] [], div [] [] )
    in
    div [ class "col-sm-4" ]
        [ h5 [ class "mt15 mb15" ] [ text "Roles" ]
        , Html.form [ class "form-horizontal" ]
            [ keyFormGroup
            , targetFormGroup
            ]
        ]


viewLoadingFormGroup : Html Msg
viewLoadingFormGroup =
    div [ class "form-group" ]
        [ label [ class "control-label col-sm-3 mr0 pr0" ]
            [ div [ class "loading--line" ] []
            ]
        , div [ class "col-sm-8" ]
            [ div [ class "loading--line" ] [] ]
        ]


viewKeyFormGroup : ColumnInfo -> Html Msg
viewKeyFormGroup key =
    div [ class "form-group" ]
        [ label [ class "control-label col-sm-3 mr0 pr0" ]
            [ text "Key"
            ]
        , div [ class "col-sm-8" ]
            -- todo: what is there is no key?
            -- not all datasets have a key at all.
            [ p [ class "mb5", style [ ( "padding", "7px 10px 0;" ) ] ] [ text key.metadata.name ]
            , p [ class "small color-mediumGray" ] [ text "The key role can only be set when importing a new dataset." ]
            ]
        ]


viewTargetFormGroup : Maybe ColumnInfo -> Html Msg
viewTargetFormGroup target =
    let
        targetText =
            target |> Maybe.map (\t -> t.metadata.name) |> Maybe.withDefault ""
    in
    div [ class "form-group" ]
        [ label [ class "control-label col-sm-3 mr0 pr0" ] [ text "Target" ]
        , div [ class "col-sm-8" ]
            --todo : this is probably supposed to be some other kind of control.
            [ input [ type_ "text", class "form-control", value targetText ] []
            ]
        ]


viewDetailsCol : Model -> Html Msg
viewDetailsCol model =
    let
        ( size, shape, created, modified ) =
            case model.dataSetResponse of
                Remote.Success resp ->
                    let
                        sizeDisplay =
                            text <| toString resp.dataSetSize ++ "B"

                        shapeDisplay =
                            text <| toString resp.totalCount ++ "x" ++ toString (List.length resp.columns)

                        createdDisplay =
                            text "?"

                        modifiedDisplay =
                            text "?"
                    in
                    ( sizeDisplay, shapeDisplay, createdDisplay, modifiedDisplay )

                Remote.Loading ->
                    let
                        loading =
                            -- this text node has magic \x00A0 characters instead of just spaces.
                            span [ class "loading--line" ] [ text "       " ]
                    in
                    ( loading, loading, loading, loading )

                _ ->
                    let
                        empty =
                            div [] []
                    in
                    ( empty, empty, empty, empty )
    in
    div [ class "col-sm-5" ]
        [ h5 [ class "mt15 mb15" ] [ text "Details" ]
        , p []
            [ strong [] [ text "Size: " ]
            , size
            ]
        , p []
            [ strong [] [ text "Shape: " ]
            , shape
            ]
        , p []
            [ strong [] [ text "Created: " ]
            , created
            ]
        , p []
            [ strong [] [ text "Modified: " ]
            , modified
            ]
        ]


viewRelatedCol : Model -> Html Msg
viewRelatedCol model =
    div [ class "col-sm-3", id "related" ]
        [ h5 [ class "mt15 mb15" ] [ text "Related" ]

        -- todo : accordion thing
        , div [] []
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
