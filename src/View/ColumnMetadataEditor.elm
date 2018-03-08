module View.ColumnMetadataEditor exposing (ExternalMsg(..), Model, Msg, init, update, updateDataSetResponse, view, viewTargetAndKeyColumns)

import Autocomplete
import Char
import Data.Columns as Columns exposing (ColumnMetadata, DataType(..), Role(..), enumDataType, enumRole)
import Data.Config exposing (Config)
import Data.DataSet as DataSet exposing (ColumnStats, ColumnStatsDict, DataSetData, DataSetName, DataSetStats, dataSetNameToString, toDataSetName)
import Data.ImputationStrategy exposing (ImputationStrategy(..), enumImputationStrategy)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Extra as ListX
import RemoteData as Remote
import Request.DataSet
import Request.Log exposing (logHttpError)
import SelectWithStyle as UnionSelect
import Table
import Util exposing ((=>), commaFormatInteger, formatFloatToString, styledNumber)
import VegaLite exposing (Spec)
import View.Extra exposing (viewIf)
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


type alias Model =
    { columnMetadata : Remote.WebData ColumnMetadataListing
    , statsResponse : Remote.WebData DataSetStats
    , dataSetName : DataSetName
    , tableState : Table.State
    , config : Config
    , modifiedMetadata : List ColumnMetadata
    , autoState : Autocomplete.State
    , targetQuery : String
    , showAutocomplete : Bool
    }


type ExternalMsg
    = NoOp
    | Updated


type alias ColumnMetadataListing =
    { pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    , metadata : List ColumnMetadata
    }


type Msg
    = StatsResponse (Remote.WebData DataSetStats)
    | SetTableState Table.State
    | ChangePage Int
    | ChangePageSize Int
    | TypeSelectionChanged ColumnMetadata DataType
    | RoleSelectionChanged ColumnMetadata Role
    | ImputationSelectionChanged ColumnMetadata ImputationStrategy
    | SetAutoCompleteState Autocomplete.Msg
    | SetTarget String
    | SetQuery String


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    Model Remote.Loading Remote.Loading dataSetName (Table.initialSort "columnName") config [] Autocomplete.empty "" False
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
    , metadata = columns
    }


updateDataSetResponse : Model -> Remote.WebData DataSetData -> ( Model, Cmd Msg )
updateDataSetResponse model dataSetResponse =
    let
        mappedColumns =
            Remote.map (.columns >> mapColumnListToPagedListing) dataSetResponse

        targetName =
            mappedColumns
                |> Remote.map .metadata
                |> Remote.withDefault []
                |> ListX.find (\m -> m.role == Columns.Target)
                |> Maybe.map .name
                |> Maybe.withDefault ""
    in
    { model | columnMetadata = mappedColumns, targetQuery = targetName, showAutocomplete = False }
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
                    { model | statsResponse = resp } => Cmd.none => NoOp

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

        SetQuery query ->
            let
                showAutocomplete =
                    not << List.isEmpty <| filterColumnNames query model.columnMetadata
            in
            { model | targetQuery = query, showAutocomplete = showAutocomplete } => Cmd.none => NoOp

        SetAutoCompleteState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update autocompleteUpdateConfig autoMsg 5 model.autoState (filterColumnNames model.targetQuery model.columnMetadata)

                newModel =
                    { model | autoState = newState }
            in
            case maybeMsg of
                Nothing ->
                    newModel => Cmd.none => NoOp

                Just updateMsg ->
                    update updateMsg newModel

        SetTarget targetName ->
            let
                ( newTarget, oldTarget ) =
                    model.columnMetadata
                        |> Remote.map
                            (\cm ->
                                let
                                    newTarget =
                                        cm.metadata
                                            |> ListX.find (\c -> c.name == targetName)

                                    oldTarget =
                                        cm.metadata
                                            |> ListX.find (\c -> c.role == Target)
                                in
                                ( newTarget, oldTarget )
                            )
                        |> Remote.withDefault ( Nothing, Nothing )

                metadataWithNew =
                    newTarget
                        |> Maybe.map
                            (\new ->
                                updateRole (getExistingOrOriginalColumn model.modifiedMetadata new) Target
                                    |> maybeAppendColumn model.modifiedMetadata
                            )
                        |> Maybe.withDefault model.modifiedMetadata

                updatedMetadata =
                    oldTarget
                        |> Maybe.map
                            (\old ->
                                updateRole (getExistingOrOriginalColumn metadataWithNew old) Feature
                                    |> maybeAppendColumn metadataWithNew
                            )
                        |> Maybe.withDefault metadataWithNew

                targetName =
                    newTarget
                        |> Maybe.map .name
                        |> Maybe.withDefault ""
            in
            { model | modifiedMetadata = updatedMetadata, targetQuery = targetName, showAutocomplete = False } => Cmd.none => NoOp


filterColumnNames : String -> Remote.WebData ColumnMetadataListing -> List ColumnMetadata
filterColumnNames query columnMetadata =
    let
        columns =
            columnMetadata |> Remote.map (\cm -> cm.metadata) |> Remote.withDefault []

        lowerQuery =
            String.toLower query
    in
    List.filter (String.contains lowerQuery << String.toLower << .name) columns


onKeyDown : Char.KeyCode -> Maybe String -> Maybe Msg
onKeyDown code maybeId =
    if code == 38 || code == 40 then
        Nothing
    else if code == 13 then
        Maybe.map SetTarget maybeId
    else
        Nothing


autocompleteUpdateConfig : Autocomplete.UpdateConfig Msg ColumnMetadata
autocompleteUpdateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown = onKeyDown
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SetTarget id
        , separateSelections = False
        }


getExistingOrOriginalColumn : List ColumnMetadata -> ColumnMetadata -> ColumnMetadata
getExistingOrOriginalColumn modifiedList column =
    modifiedList
        |> ListX.find (\a -> a.name == column.name)
        |> Maybe.withDefault column


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


updateColumnMetadata : List ColumnMetadata -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd msg )
updateColumnMetadata info columnListing =
    { columnListing | metadata = info } => Cmd.none


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
    let
        stats =
            model.statsResponse
                |> Remote.map (\sr -> sr.columns)
                |> Remote.withDefault Dict.empty

        mergedMetadata =
            model.columnMetadata
                |> Remote.map
                    (\cm ->
                        let
                            modifiedDict =
                                model.modifiedMetadata
                                    |> List.map (\m -> ( m.name, m ))
                                    |> Dict.fromList

                            unionedMetadata =
                                cm.metadata
                                    |> List.map (\m -> ( m.name, m ))
                                    |> Dict.fromList
                                    |> Dict.union modifiedDict
                                    |> Dict.toList
                                    |> List.map Tuple.second
                        in
                        { cm | metadata = unionedMetadata }
                    )
    in
    div []
        [ div [ class "row mb25" ]
            [ div [ class "col-sm-3" ]
                [ h3 [] [ text "Columns" ] ]
            , div [ class "col-sm-2 col-sm-offset-7 right" ]
                [ PageSize.view ChangePageSize ]
            ]
        , Grid.view filterColumnsToDisplay (config model.config.toolTips stats) model.tableState mergedMetadata
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
                            resp.metadata
                                |> ListX.find (\m -> m.role == Columns.Key)
                                |> Maybe.map viewKeyFormGroup
                                |> Maybe.withDefault (div [] [])
                    in
                    ( keyGroup, viewTargetFormGroup model )

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


viewKeyFormGroup : ColumnMetadata -> Html Msg
viewKeyFormGroup key =
    div [ class "form-group" ]
        [ label [ class "control-label col-sm-3 mr0 pr0" ]
            [ text "Key"
            ]
        , div [ class "col-sm-8" ]
            [ p [ class "mb5", style [ ( "padding", "7px 10px 0;" ) ] ] [ text key.name ]
            , p [ class "small color-mediumGray" ] [ text "The key role can only be set when importing a new dataset." ]
            ]
        ]


viewTargetFormGroup : Model -> Html Msg
viewTargetFormGroup model =
    div [ class "form-group" ]
        [ label [ class "control-label col-sm-3 mr0 pr0" ] [ text "Target" ]
        , div [ class "col-sm-8" ]
            [ input [ type_ "text", class "form-control", value model.targetQuery, onInput SetQuery ] []
            , viewIf (\() -> Html.map SetAutoCompleteState (Autocomplete.view viewConfig 5 model.autoState (filterColumnNames model.targetQuery model.columnMetadata))) model.showAutocomplete
            ]
        ]


viewConfig : Autocomplete.ViewConfig ColumnMetadata
viewConfig =
    let
        customizedLi keySelected mouseSelected person =
            { attributes =
                [ classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected )
                    , ( "mouse-selected", mouseSelected )
                    ]
                ]
            , children = [ Html.text person.name ]
            }
    in
    Autocomplete.viewConfig
        { toId = .name
        , ul = [ class "autocomplete-list" ]
        , li = customizedLi
        }


filterColumnsToDisplay : ColumnMetadataListing -> List ColumnMetadata
filterColumnsToDisplay columnListing =
    let
        drop =
            columnListing.pageSize * columnListing.pageNumber
    in
    columnListing.metadata
        |> List.drop drop
        |> List.take columnListing.pageSize


config : Dict String String -> ColumnStatsDict -> Grid.Config ColumnMetadata Msg
config toolTips stats =
    let
        makeIcon =
            helpIcon toolTips
    in
    Grid.config
        { toId = \c -> c.name
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , typeColumn makeIcon
            , roleColumn makeIcon
            , imputationColumn makeIcon
            , statsColumn stats
            ]
        }


nameColumn : Grid.Column ColumnMetadata Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Column Name"
        , viewData = columnNameCell
        , sorter = Table.increasingOrDecreasingBy (\c -> c.name)
        , headAttributes = [ class "left per25" ]
        , headHtml = []
        }


columnNameCell : ColumnMetadata -> Table.HtmlDetails Msg
columnNameCell column =
    Table.HtmlDetails [ class "name" ]
        [ text column.name ]


typeColumn : (String -> List (Html Msg)) -> Grid.Column ColumnMetadata Msg
typeColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Type"
        , viewData = dataTypeCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Type"
        }


dataTypeCell : ColumnMetadata -> Table.HtmlDetails Msg
dataTypeCell column =
    Table.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumDataType (TypeSelectionChanged column) column.dataType ]


roleColumn : (String -> List (Html Msg)) -> Grid.Column ColumnMetadata Msg
roleColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Role"
        , viewData = roleCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Role"
        }


roleCell : ColumnMetadata -> Table.HtmlDetails Msg
roleCell column =
    Table.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumRole (RoleSelectionChanged column) column.role ]


enumOption : e -> e -> Html Msg
enumOption roleOption roleModel =
    option [ selected (roleOption == roleModel) ] [ text (toString roleOption) ]


imputationColumn : (String -> List (Html Msg)) -> Grid.Column ColumnMetadata Msg
imputationColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Imputation"
        , viewData = imputationCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Imputation"
        }


imputationCell : ColumnMetadata -> Table.HtmlDetails Msg
imputationCell column =
    Table.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumImputationStrategy (ImputationSelectionChanged column) column.imputation ]


statsColumn : ColumnStatsDict -> Grid.Column ColumnMetadata Msg
statsColumn stats =
    Grid.veryCustomColumn
        { name = "Stats"
        , viewData = statsCell stats
        , sorter = Table.unsortable
        , headAttributes = [ class "per20", colspan 2 ]
        , headHtml = []
        }


statsCell : ColumnStatsDict -> ColumnMetadata -> Table.HtmlDetails Msg
statsCell stats column =
    let
        columnStats =
            Dict.get column.name stats
    in
    Table.HtmlDetails [ class "stats" ]
        [ statsDisplay columnStats ]


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


histogramColumn : Grid.Column ColumnMetadata Msg
histogramColumn =
    Grid.veryCustomColumn
        { name = "Distribution"
        , viewData = histogram
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


histogram : ColumnMetadata -> Table.HtmlDetails Msg
histogram column =
    Table.HtmlDetails []
        [ div [ id ("histogram_" ++ column.name) ] [] ]
