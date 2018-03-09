module View.ColumnMetadataEditor exposing (ExternalMsg(..), Model, Msg, init, update, updateDataSetResponse, view, viewTargetAndKeyColumns)

import Data.Columns as Columns exposing (ColumnMetadata, DataType(..), Role(..), enumDataType, enumRole)
import Data.Config exposing (Config)
import Data.DataSet as DataSet exposing (ColumnStats, ColumnStatsDict, DataSetData, DataSetName, DataSetStats, dataSetNameToString, toDataSetName)
import Data.ImputationStrategy exposing (ImputationStrategy(..), enumImputationStrategy)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as ListX
import RemoteData as Remote
import Request.DataSet
import Request.Log exposing (logHttpError)
import SelectWithStyle as UnionSelect
import Table
import Util exposing ((=>), commaFormatInteger, formatFloatToString, styledNumber)
import VegaLite exposing (Spec)
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


type alias Model =
    { columnMetadata : Remote.WebData ColumnMetadataListing
    , dataSetName : DataSetName
    , tableState : Table.State
    , config : Config
    , modifiedMetadata : List ColumnMetadata
    }


type ExternalMsg
    = NoOp
    | Updated


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
    | ChangePageSize Int
    | TypeSelectionChanged ColumnMetadata DataType
    | RoleSelectionChanged ColumnMetadata Role
    | ImputationSelectionChanged ColumnMetadata ImputationStrategy


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    Model Remote.Loading dataSetName (Table.initialSort "columnName") config []
        => Cmd.none


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


updateDataSetResponse : Model -> Remote.WebData DataSetData -> ( Model, Cmd Msg )
updateDataSetResponse model dataSetResponse =
    let
        mappedColumns =
            Remote.map (.columns >> mapColumnListToPagedListing) dataSetResponse
    in
    { model | columnMetadata = mappedColumns }
        => (Request.DataSet.getStats model.config model.dataSetName
                |> Remote.sendRequest
                |> Cmd.map StatsResponse
           )


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        StatsResponse resp ->
            case resp of
                Remote.Success s ->
                    let
                        updatedColumnInfo =
                            Remote.map2 mergeListingAndStats model.columnMetadata resp
                    in
                    { model | columnMetadata = updatedColumnInfo } => Cmd.none => NoOp

                Remote.Failure err ->
                    model => logHttpError err => NoOp

                _ ->
                    model => Cmd.none => NoOp

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none
                => NoOp

        ChangePage pageNumber ->
            let
                ( columnListing, cmd ) =
                    Remote.update (updateColumnPageNumber pageNumber) model.columnMetadata
            in
            { model | columnMetadata = columnListing } => cmd => NoOp

        ChangePageSize pageSize ->
            let
                ( columnListing, cmd ) =
                    Remote.update (updateColumnPageSize pageSize) model.columnMetadata
            in
            { model | columnMetadata = columnListing } => cmd => NoOp

        RoleSelectionChanged metadata selection ->
            { model
                | modifiedMetadata =
                    updateRole (getExistingOrOriginalColumn model.modifiedMetadata metadata) selection
                        |> maybeAppendColumn model.modifiedMetadata
            }
                => Cmd.none
                => Updated

        TypeSelectionChanged metadata selection ->
            { model
                | modifiedMetadata =
                    updateDataType (getExistingOrOriginalColumn model.modifiedMetadata metadata) selection
                        |> maybeAppendColumn model.modifiedMetadata
            }
                => Cmd.none
                => Updated

        ImputationSelectionChanged metadata selection ->
            { model
                | modifiedMetadata =
                    updateImputation (getExistingOrOriginalColumn model.modifiedMetadata metadata) selection
                        |> maybeAppendColumn model.modifiedMetadata
            }
                => Cmd.none
                => Updated


getExistingOrOriginalColumn : List ColumnMetadata -> ColumnMetadata -> ColumnMetadata
getExistingOrOriginalColumn modifiedList column =
    case ListX.find (\a -> a.name == column.name) modifiedList of
        Just existing ->
            existing

        Nothing ->
            column


updateImputation : ColumnMetadata -> ImputationStrategy -> ( ColumnMetadata, Bool )
updateImputation column value =
    { column | imputation = value } => column.imputation == value


updateRole : ColumnMetadata -> Role -> ( ColumnMetadata, Bool )
updateRole column value =
    { column | role = value } => column.role == value


updateDataType : ColumnMetadata -> DataType -> ( ColumnMetadata, Bool )
updateDataType column value =
    { column | dataType = value } => column.dataType == value


maybeAppendColumn : List ColumnMetadata -> ( ColumnMetadata, Bool ) -> List ColumnMetadata
maybeAppendColumn list ( metadata, unchanged ) =
    if unchanged then
        list
    else
        case ListX.find (\a -> a.name == metadata.name) list of
            Just col ->
                ListX.replaceIf (\a -> a.name == metadata.name) metadata list

            Nothing ->
                List.append list [ metadata ]


updateColumnPageNumber : Int -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd Msg )
updateColumnPageNumber pageNumber columnListing =
    { columnListing | pageNumber = pageNumber } => Cmd.none


updateColumnPageSize : Int -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd Msg )
updateColumnPageSize pageSize columnListing =
    { columnListing | pageSize = pageSize, pageNumber = 0, totalPages = columnListing.totalCount // pageSize } => Cmd.none


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
    div []
        [ div [ class "row mb25" ]
            [ div [ class "col-sm-3" ]
                [ h3 [] [ text "Columns" ] ]
            , div [ class "col-sm-2 col-sm-offset-7 right" ]
                [ PageSize.view ChangePageSize 10 ]
            ]
        , Grid.view filterColumnsToDisplay (config model.config.toolTips) model.tableState model.columnMetadata
        , div [ class "center" ] [ Pager.view model.columnMetadata ChangePage ]
        ]


viewTargetAndKeyColumns : Model -> Html Msg
viewTargetAndKeyColumns model =
    let
        ( keyFormGroup, targetFormGroup ) =
            case model.columnMetadata of
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
    Html.form [ class "form-horizontal" ]
        [ keyFormGroup
        , targetFormGroup
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
    Table.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumDataType (TypeSelectionChanged column.metadata) column.metadata.dataType ]


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
    Table.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumRole (RoleSelectionChanged column.metadata) column.metadata.role ]


enumOption : e -> e -> Html Msg
enumOption roleOption roleModel =
    option [ selected (roleOption == roleModel) ] [ text (toString roleOption) ]


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
    Table.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumImputationStrategy (ImputationSelectionChanged column.metadata) column.metadata.imputation ]


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
                    , styledNumber <| formatFloatToString stats.min
                    , br [] []
                    , strong [] [ text "Max: " ]
                    , styledNumber <| formatFloatToString stats.max
                    , br [] []
                    , strong [] [ text "Standard Deviation: " ]
                    , styledNumber <| formatFloatToString stats.stddev
                    , br [] []
                    , strong [ class "text-danger" ] [ text "Errors: " ]
                    , styledNumber <| commaFormatInteger stats.errors
                    ]
                , div [ class "col-sm-6 pl0 pr0" ]
                    [ strong [] [ text "Value Count: " ]
                    , styledNumber <| commaFormatInteger stats.row_count
                    , br [] []
                    , strong [ class "text-danger" ] [ text "# Missing: " ]
                    , styledNumber <| commaFormatInteger stats.missing
                    , br [] []
                    , strong [] [ text "Mean: " ]
                    , styledNumber <| formatFloatToString stats.mean
                    , br [] []
                    , strong [] [ text "Median: " ]
                    , styledNumber <| formatFloatToString stats.median
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
