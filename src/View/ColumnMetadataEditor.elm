module View.ColumnMetadataEditor exposing (ExternalMsg(..), Model, Msg, init, subscriptions, update, updateDataSetResponse, view, viewTargetAndKeyColumns)

import Autocomplete
import Char
import Data.Columns as Columns exposing (enumDataType, enumRole)
import Data.Config as Config
import Data.Context exposing (ContextModel)
import Data.ImputationStrategy exposing (enumImputationStrategy)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput)
import Http
import Json.Decode as Decode
import Nexosis.Api.Data
import Nexosis.Types.Columns as Columns exposing (ColumnMetadata, ColumnStats, ColumnStatsDict, DataType(..), Role(..))
import Nexosis.Types.DataSet as DataSet exposing (DataSetData, DataSetName, DataSetStats, toDataSetName)
import Nexosis.Types.ImputationStrategy exposing (ImputationStrategy(..))
import NexosisHelpers exposing (SortDirection(..), SortParameters, commaFormatInteger, formatFloatToString)
import Ports
import RemoteData as Remote
import Request.DataSet
import Request.Log exposing (logHttpError)
import SelectWithStyle as UnionSelect
import StateStorage exposing (saveAppState)
import String.Extra as String
import Task
import Util exposing ((=>), delayTask, formatDisplayName, isJust, spinner, styledNumber)
import VegaLite exposing (Spec, combineSpecs)
import View.Charts exposing (distributionHistogram, wordOccurrenceTable)
import View.Error exposing (viewRemoteError)
import View.Extra exposing (viewIf)
import View.Grid as Grid exposing (defaultCustomizations)
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


type alias Model =
    { columnMetadata : Remote.WebData ColumnMetadataListing
    , statsResponse : Dict String (Remote.WebData ColumnStats)
    , dataSetName : DataSetName
    , tableState : Grid.State
    , modifiedMetadata : Dict String ColumnMetadata
    , changesPendingSave : Dict String ColumnMetadata
    , autoState : Autocomplete.State
    , targetQuery : String
    , previewTarget : Maybe String
    , showAutocomplete : Bool
    , showTarget : Bool
    , columnInEditMode : Maybe ColumnMetadata
    , saveResult : Remote.WebData ()
    }


type ExternalMsg
    = NoOp
    | Updated (List ColumnMetadata)


type alias ColumnMetadataListing =
    { pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    , metadata : Dict String ColumnMetadata
    }


type Msg
    = StatsResponse (Remote.WebData DataSetStats)
    | SingleStatsResponse String (Remote.WebData DataSetStats)
    | SetTableState Grid.State
    | ChangePage Int
    | ChangePageSize Int
    | TypeSelectionChanged DataType
    | RoleSelectionChanged Role
    | ImputationSelectionChanged ImputationStrategy
    | SetAutoCompleteState Autocomplete.Msg
    | SetTarget String
    | SetQuery String
    | PreviewTarget String
    | AutocompleteWrap Bool
    | AutocompleteReset
    | SelectColumnForEdit ColumnMetadata
    | CancelColumnEdit
    | CommitMetadataChange
    | NoOpMsg
    | SaveMetadata (Remote.WebData ())


type alias StatsDict =
    Dict String (Remote.WebData ColumnStats)


init : DataSetName -> Bool -> ( Model, Cmd Msg )
init dataSetName showTarget =
    Model Remote.Loading Dict.empty dataSetName (Grid.initialSort "columnName" Ascending) Dict.empty Dict.empty Autocomplete.empty "" Nothing False showTarget Nothing Remote.NotAsked
        => Cmd.none


mapColumnListToPagedListing : ContextModel -> List ColumnMetadata -> ColumnMetadataListing
mapColumnListToPagedListing context columns =
    let
        count =
            List.length columns

        pageSize =
            context.userPageSize
    in
    { pageNumber = 0
    , totalPages = calcTotalPages count pageSize
    , pageSize = context.userPageSize
    , totalCount = count
    , metadata = Dict.fromListBy .name columns
    }


calcTotalPages : Int -> Int -> Int
calcTotalPages count pageSize =
    (count + pageSize - 1) // pageSize


updateDataSetResponse : ContextModel -> Model -> Remote.WebData DataSetData -> ( Model, Cmd Msg )
updateDataSetResponse context model dataSetResponse =
    let
        mappedColumns =
            Remote.map (.columns >> mapColumnListToPagedListing context) dataSetResponse

        targetName =
            mappedColumns
                |> Remote.map .metadata
                |> Remote.withDefault Dict.empty
                |> Dict.find (\_ m -> m.role == Columns.Target)
                |> Maybe.map Tuple.first
                |> Maybe.withDefault ""
    in
    { model | columnMetadata = mappedColumns, targetQuery = targetName, showAutocomplete = False }
        => (Nexosis.Api.Data.getStats context.config.clientConfig model.dataSetName
                |> Remote.sendRequest
                |> Cmd.map StatsResponse
           )


delayAndRecheckStats : Config.Config -> DataSetName -> Cmd Msg
delayAndRecheckStats config dataSetName =
    delayTask 20
        |> Task.andThen (\_ -> Nexosis.Api.Data.getStats config.clientConfig dataSetName |> Http.toTask)
        |> Remote.asCmd
        |> Cmd.map StatsResponse


update : Msg -> Model -> ContextModel -> (List ColumnMetadata -> Cmd (Remote.WebData ())) -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model context pendingSaveCommand =
    case msg of
        StatsResponse resp ->
            case resp of
                Remote.Success s ->
                    let
                        stats =
                            s.columns
                                |> Dict.map (\_ columnStats -> Remote.succeed columnStats)

                        allMetadata =
                            model.columnMetadata
                                |> Remote.map
                                    (\m ->
                                        m.metadata
                                            |> Dict.values
                                            |> List.filter (\c -> c.role /= Key)
                                            |> List.length
                                    )
                                |> Remote.withDefault 0

                        reFetchStats =
                            if Dict.size stats < allMetadata then
                                delayAndRecheckStats context.config model.dataSetName
                            else
                                Cmd.none
                    in
                    { model | statsResponse = stats } => reFetchStats => NoOp

                Remote.Failure err ->
                    model => logHttpError err => NoOp

                _ ->
                    model => Cmd.none => NoOp

        SingleStatsResponse columnName resp ->
            case resp of
                Remote.Success { columns } ->
                    let
                        updatedStats =
                            columns
                                |> Dict.map (\_ columnStats -> Remote.succeed columnStats)
                                |> flip Dict.union model.statsResponse
                    in
                    { model | statsResponse = updatedStats } => Cmd.none => NoOp

                Remote.Failure err ->
                    { model | statsResponse = Dict.insert columnName (Remote.Failure err) model.statsResponse } => Cmd.none => NoOp

                _ ->
                    model => Cmd.none => NoOp

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none => NoOp

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
            { model | columnMetadata = columnListing } => Cmd.batch [ StateStorage.saveAppState { context | userPageSize = pageSize }, cmd ] => NoOp

        RoleSelectionChanged selection ->
            { model
                | columnInEditMode =
                    model.columnInEditMode
                        |> Maybe.map (updateRole selection)
            }
                => Cmd.none
                => NoOp

        TypeSelectionChanged selection ->
            { model
                | columnInEditMode =
                    model.columnInEditMode
                        |> Maybe.map (updateDataType selection)
            }
                => Cmd.none
                => NoOp

        ImputationSelectionChanged selection ->
            { model
                | columnInEditMode =
                    model.columnInEditMode
                        |> Maybe.map (updateImputation selection)
            }
                => Cmd.none
                => NoOp

        CommitMetadataChange ->
            updateMetadata model model.columnInEditMode pendingSaveCommand

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
                    update updateMsg newModel context pendingSaveCommand

        PreviewTarget targetName ->
            { model | previewTarget = Just targetName } => Cmd.none => NoOp

        AutocompleteWrap toTop ->
            case model.previewTarget of
                Just target ->
                    update AutocompleteReset model context pendingSaveCommand

                Nothing ->
                    if toTop then
                        { model
                            | autoState = Autocomplete.resetToLastItem autocompleteUpdateConfig (filterColumnNames model.targetQuery model.columnMetadata) 5 model.autoState
                            , previewTarget = Maybe.map .name <| List.head <| List.reverse <| List.take 5 <| filterColumnNames model.targetQuery model.columnMetadata
                        }
                            => Cmd.none
                            => NoOp
                    else
                        { model
                            | autoState = Autocomplete.resetToFirstItem autocompleteUpdateConfig (filterColumnNames model.targetQuery model.columnMetadata) 5 model.autoState
                            , previewTarget = Maybe.map .name <| List.head <| List.take 5 <| filterColumnNames model.targetQuery model.columnMetadata
                        }
                            => Cmd.none
                            => NoOp

        AutocompleteReset ->
            { model | autoState = Autocomplete.reset autocompleteUpdateConfig model.autoState, previewTarget = Nothing } => Cmd.none => NoOp

        SetTarget targetName ->
            let
                newTarget =
                    model.columnMetadata
                        |> Remote.map
                            (\cm ->
                                cm.metadata
                                    |> Dict.get targetName
                                    |> Maybe.map (updateRole Target)
                            )
                        |> Remote.withDefault Nothing
            in
            updateMetadata model newTarget pendingSaveCommand

        SelectColumnForEdit column ->
            { model | columnInEditMode = Just column, saveResult = Remote.NotAsked } => Cmd.none => NoOp

        CancelColumnEdit ->
            { model | columnInEditMode = Nothing, changesPendingSave = Dict.empty, saveResult = Remote.NotAsked } => Cmd.none => NoOp

        NoOpMsg ->
            model => Cmd.none => NoOp

        SaveMetadata result ->
            if Remote.isSuccess result then
                let
                    columnsWithNewDataType =
                        model.changesPendingSave
                            |> Dict.values
                            |> List.filter
                                (\c ->
                                    model.columnMetadata
                                        |> Remote.map (\cm -> cm.metadata)
                                        |> Remote.withDefault Dict.empty
                                        |> Dict.union model.modifiedMetadata
                                        |> Dict.get c.name
                                        |> Maybe.map (\old -> old.dataType /= c.dataType)
                                        |> Maybe.withDefault False
                                )

                    statsBeingRequested =
                        columnsWithNewDataType
                            |> List.map (\c -> ( c.name, Remote.Loading ))
                            |> Dict.fromList
                            |> flip Dict.union model.statsResponse

                    modifiedMetadata =
                        Dict.union model.changesPendingSave model.modifiedMetadata
                in
                { model | modifiedMetadata = modifiedMetadata, changesPendingSave = Dict.empty, saveResult = result, columnInEditMode = Nothing, showAutocomplete = False, previewTarget = Nothing, statsResponse = statsBeingRequested }
                    => Cmd.batch
                        (Ports.highlightIds
                            (model.changesPendingSave |> Dict.keys |> List.map (\c -> "column_" ++ c |> String.classify))
                            :: List.map
                                (\c ->
                                    Nexosis.Api.Data.getStatsForColumn context.config.clientConfig model.dataSetName c.name c.dataType
                                        |> Remote.sendRequest
                                        |> Cmd.map (SingleStatsResponse c.name)
                                )
                                columnsWithNewDataType
                        )
                    => Updated (Dict.values model.changesPendingSave)
            else
                { model | saveResult = result } => Cmd.none => NoOp


updateMetadata : Model -> Maybe ColumnMetadata -> (List ColumnMetadata -> Cmd (Remote.WebData ())) -> ( ( Model, Cmd Msg ), ExternalMsg )
updateMetadata model updatedColumn saveCommand =
    let
        existingColumns =
            model.columnMetadata
                |> Remote.map .metadata
                |> Remote.withDefault Dict.empty

        ensureSingleTarget newColumn =
            if newColumn.role == Target then
                existingColumns
                    |> Dict.union model.modifiedMetadata
                    |> Dict.find (\_ c -> c.role == Target)
                    |> Maybe.map (\( _, oldTarget ) -> Just [ updateRole Feature oldTarget, newColumn ])
                    |> Maybe.withDefault (Just [ newColumn ])
            else
                Just [ newColumn ]

        changedMetadata =
            updatedColumn
                |> Maybe.andThen ensureSingleTarget
                |> Maybe.withDefault []
                |> Dict.fromListBy .name

        modifiedMetadata =
            Dict.intersect model.modifiedMetadata existingColumns
                |> Dict.union changedMetadata

        targetName =
            existingColumns
                |> Dict.union modifiedMetadata
                |> Dict.find (\_ c -> c.role == Target)
                |> Maybe.map (\( _, c ) -> c.name)
                |> Maybe.withDefault ""
    in
    if modifiedMetadata /= Dict.empty then
        { model | changesPendingSave = changedMetadata, targetQuery = targetName, saveResult = Remote.Loading } => Cmd.map SaveMetadata (saveCommand <| Dict.values changedMetadata) => NoOp
    else
        { model | modifiedMetadata = modifiedMetadata, columnInEditMode = Nothing, targetQuery = targetName, saveResult = Remote.NotAsked, showAutocomplete = False, previewTarget = Nothing } => Cmd.none => NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Autocomplete.subscription |> Sub.map SetAutoCompleteState


filterColumnNames : String -> Remote.WebData ColumnMetadataListing -> List ColumnMetadata
filterColumnNames query columnMetadata =
    let
        columns =
            columnMetadata |> Remote.map (\cm -> Dict.values cm.metadata) |> Remote.withDefault []

        lowerQuery =
            String.toLower query
    in
    List.filter (String.contains lowerQuery << String.toLower << .name) columns


onKeyDown : Char.KeyCode -> Maybe String -> Maybe Msg
onKeyDown code maybeId =
    if code == 38 || code == 40 then
        -- up & down arrows
        Maybe.map PreviewTarget maybeId
    else if code == 13 || code == 9 then
        -- enter & tab
        Maybe.map SetTarget maybeId
    else if code == 27 then
        --escape
        Just <| SetQuery ""
    else
        Nothing


autocompleteUpdateConfig : Autocomplete.UpdateConfig Msg ColumnMetadata
autocompleteUpdateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown = onKeyDown
        , onTooLow = Just <| AutocompleteWrap True
        , onTooHigh = Just <| AutocompleteWrap False
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SetTarget id
        , separateSelections = False
        }


updateImputation : ImputationStrategy -> ColumnMetadata -> ColumnMetadata
updateImputation value column =
    { column | imputation = value }


updateRole : Role -> ColumnMetadata -> ColumnMetadata
updateRole value column =
    { column | role = value }


updateDataType : DataType -> ColumnMetadata -> ColumnMetadata
updateDataType value column =
    { column | dataType = value }


updateColumnPageNumber : Int -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd Msg )
updateColumnPageNumber pageNumber columnListing =
    { columnListing | pageNumber = pageNumber } => Cmd.none


updateColumnPageSize : Int -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd Msg )
updateColumnPageSize pageSize columnListing =
    { columnListing | pageSize = pageSize, pageNumber = 0, totalPages = calcTotalPages columnListing.totalCount pageSize } => Cmd.none


updateColumnMetadata : Dict String ColumnMetadata -> ColumnMetadataListing -> ( ColumnMetadataListing, Cmd msg )
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


view : ContextModel -> Model -> Html Msg
view context model =
    let
        mergedMetadata =
            model.columnMetadata
                |> Remote.map
                    (\cm ->
                        let
                            unionedMetadata =
                                cm.metadata
                                    |> Dict.union model.modifiedMetadata
                        in
                        { cm | metadata = unionedMetadata }
                    )

        editTable =
            buildEditTable context model.statsResponse model
    in
    div []
        [ div [ class "row mb25" ]
            [ div [ class "col-sm-3 pleft0" ]
                [ h3 [] [ text "Column metadata" ] ]
            , div [ class "col-sm-2 col-sm-offset-7 right" ]
                [ PageSize.view ChangePageSize context.userPageSize ]
            ]
        , Grid.view filterColumnsToDisplay (config context.config.toolTips model.statsResponse editTable model.columnInEditMode) model.tableState mergedMetadata
        , div [ class "center" ] [ Pager.view model.columnMetadata ChangePage ]
        ]


buildEditTable : ContextModel -> StatsDict -> Model -> ColumnMetadata -> Html Msg
buildEditTable context stats model column =
    let
        saveButton =
            case model.saveResult of
                Remote.Loading ->
                    button [ class "btn btn-danger btn-sm" ] [ spinner ]

                _ ->
                    button [ class "btn btn-danger btn-sm", onClick CommitMetadataChange ] [ text "Save Changes" ]
    in
    tr [ class "modal fade in", style [ ( "display", "table-row" ), ( "position", "static" ) ] ]
        [ td [ class "p0", colspan 6 ]
            [ div [ class "modal-dialog modal-content metadata-editor m0", style [ ( "z-index", "1050" ), ( "width", "auto" ) ] ]
                [ Grid.view identity (editConfig context.config.toolTips stats) Grid.initialUnsorted (Remote.succeed [ column ])
                , div [ class "text-left mr10 ml10" ] [ viewRemoteError model.saveResult ]
                , div [ class "modal-footer" ]
                    [ button [ class "btn btn-link btn-sm", onClick CancelColumnEdit ] [ text "Discard" ]
                    , saveButton
                    ]
                ]
            , div [ class "modal-backdrop in", onClick CancelColumnEdit ] []
            ]
        ]


viewEditModeOverlay : Maybe ColumnMetadata -> Html Msg
viewEditModeOverlay editColumn =
    if isJust editColumn then
        div [ class "modal-backdrop in", onClick CancelColumnEdit ] []
    else
        div [] []


viewTargetAndKeyColumns : ContextModel -> Model -> Html Msg
viewTargetAndKeyColumns context model =
    let
        ( keyFormGroup, targetFormGroup ) =
            case model.columnMetadata of
                Remote.Success resp ->
                    let
                        keyGroup =
                            resp.metadata
                                |> Dict.find (\_ m -> m.role == Columns.Key)
                                |> Maybe.map (viewKeyFormGroup << Tuple.second)
                                |> Maybe.withDefault (div [] [])
                    in
                    ( keyGroup, viewTargetFormGroup context model )

                Remote.Loading ->
                    ( viewLoadingFormGroup, viewLoadingFormGroup )

                _ ->
                    ( div [] [], div [] [] )
    in
    div [ class "form-horizontal" ]
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


viewTargetFormGroup : ContextModel -> Model -> Html Msg
viewTargetFormGroup context model =
    if model.showTarget then
        let
            queryText =
                Maybe.withDefault model.targetQuery model.previewTarget
        in
        div [ class "form-group" ]
            [ label [ class "control-label col-sm-3 mr0 pr0" ] (text "Target " :: helpIcon context.config.toolTips "Target")
            , div [ class "col-sm-8" ]
                [ input [ type_ "text", class "form-control", value queryText, onInput SetQuery ] []
                , viewIf
                    (\() ->
                        div [ class "autocomplete-menu" ] [ Html.map SetAutoCompleteState (Autocomplete.view viewConfig 5 model.autoState (filterColumnNames model.targetQuery model.columnMetadata)) ]
                    )
                    model.showAutocomplete
                , viewIf (\() -> viewRemoteError model.saveResult) <| not <| isJust model.columnInEditMode
                ]
            ]
    else
        div [] []


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
        |> Dict.values
        |> List.drop drop
        |> List.take columnListing.pageSize


config : Dict String String -> StatsDict -> (ColumnMetadata -> Html Msg) -> Maybe ColumnMetadata -> Grid.Config ColumnMetadata Msg
config toolTips stats buildEditTable columnInEdit =
    let
        makeIcon =
            helpIcon toolTips

        columns =
            [ nameColumn
            , typeColumn makeIcon |> (\c -> { c | viewData = lockedDropdownCell (\v -> toString v.dataType) })
            , roleColumn makeIcon |> (\c -> { c | viewData = lockedDropdownCell (\v -> toString v.role) })
            , imputationColumn makeIcon |> (\c -> { c | viewData = stringImputationCell (\v -> toString v.imputation) })
            , statsColumn stats
            , histogramColumn stats
            ]
    in
    Grid.configCustom
        { toId = \c -> c.name
        , toMsg = SetTableState
        , columns = columns |> List.map Grid.veryCustomColumn
        , customizations =
            \defaults ->
                { defaults
                    | rowAttrs = customRowAttributes
                    , rowOverride = rowOverride buildEditTable columnInEdit
                    , tableAttrs = metadataTableAttrs
                }
        }


editConfig : Dict String String -> StatsDict -> Grid.Config ColumnMetadata Msg
editConfig toolTips stats =
    let
        makeIcon =
            helpIcon toolTips

        columns =
            [ { nameColumn | sorter = Grid.unsortable }
            , typeColumn makeIcon
            , roleColumn makeIcon
            , imputationColumn makeIcon
            , statsColumn stats
            , histogramColumn stats
            ]
    in
    Grid.configCustom
        { toId = \c -> c.name
        , toMsg = SetTableState
        , columns = columns |> List.map Grid.veryCustomColumn
        , customizations = \defaults -> { defaults | tableAttrs = [ class "table table-striped mb0" ] }
        }


metadataTableAttrs : List (Attribute msg)
metadataTableAttrs =
    [ id "metadata", class "table table-striped" ]


rowOverride : (ColumnMetadata -> Html Msg) -> Maybe ColumnMetadata -> ( ColumnMetadata -> Bool, ColumnMetadata -> Html Msg )
rowOverride buildEditTable columnInEdit =
    case columnInEdit of
        Just column ->
            ( \c -> c.name == column.name, \c -> buildEditTable c )

        Nothing ->
            ( \_ -> False, \_ -> div [] [] )


lockedDropdownCell : (ColumnMetadata -> String) -> ColumnMetadata -> Grid.HtmlDetails Msg
lockedDropdownCell getValue column =
    if column.role == Key then
        if getValue column == "Key" then
            Grid.HtmlDetails [ class "form-group" ] [ select [ disabled True, class "form-control" ] [ option [] [ text "Key" ] ] ]
        else
            emptyDropdown
    else
        Grid.HtmlDetails [ class "form-group" ]
            [ select
                [ class "form-control"
                , Html.Events.onWithOptions "mousedown"
                    { preventDefault = True
                    , stopPropagation = False
                    }
                    (Decode.succeed NoOpMsg)
                ]
                [ option [] [ text <| getValue column ] ]
            ]


stringImputationCell : (ColumnMetadata -> String) -> ColumnMetadata -> Grid.HtmlDetails Msg
stringImputationCell getValue column =
    if column.dataType == Text then
        naDropdown
    else
        lockedDropdownCell getValue column


customRowAttributes : ColumnMetadata -> List (Attribute Msg)
customRowAttributes column =
    if column.role == Key then
        [ id "key" ]
    else
        [ id <| String.classify <| "column_" ++ column.name, onClick <| SelectColumnForEdit column ]


type alias ColumnDisplayProperties =
    { headAttributes : List (Attribute Msg)
    , headHtml : List (Html Msg)
    , name : String
    , sorter : Grid.Sorter ColumnMetadata
    , viewData : ColumnMetadata -> Grid.HtmlDetails Msg
    }


nameColumn : ColumnDisplayProperties
nameColumn =
    { name = "Column Name"
    , viewData = \c -> Grid.HtmlDetails [ class "name" ] [ text <| formatDisplayName c.name ]
    , sorter = Grid.increasingOrDecreasingBy (\c -> c.name)
    , headAttributes = [ class "left per30" ]
    , headHtml = []
    }


typeColumn : (String -> List (Html Msg)) -> ColumnDisplayProperties
typeColumn makeIcon =
    { name = "Type"
    , viewData = dataTypeCell
    , sorter = Grid.unsortable
    , headAttributes = [ class "per10" ]
    , headHtml = text "Type " :: makeIcon "Type"
    }


dataTypeCell : ColumnMetadata -> Grid.HtmlDetails Msg
dataTypeCell column =
    Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected [ class "form-control" ] enumDataType TypeSelectionChanged column.dataType ]


emptyDropdown : Grid.HtmlDetails Msg
emptyDropdown =
    Grid.HtmlDetails [ class "form-group" ] [ select [ disabled True, class "form-control" ] [] ]


naDropdown : Grid.HtmlDetails Msg
naDropdown =
    Grid.HtmlDetails [ class "form-group color-mediumGray" ] [ text "N/A" ]


roleColumn : (String -> List (Html Msg)) -> ColumnDisplayProperties
roleColumn makeIcon =
    { name = "Role"
    , viewData = roleCell
    , sorter = Grid.unsortable
    , headAttributes = [ class "per10" ]
    , headHtml = text "Role " :: makeIcon "Role"
    }


roleCell : ColumnMetadata -> Grid.HtmlDetails Msg
roleCell column =
    Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected [ class "form-control" ] enumRole RoleSelectionChanged column.role ]


imputationColumn : (String -> List (Html Msg)) -> ColumnDisplayProperties
imputationColumn makeIcon =
    { name = "Imputation"
    , viewData = imputationCell
    , sorter = Grid.unsortable
    , headAttributes = [ class "per10" ]
    , headHtml = text "Imputation " :: makeIcon "Imputation"
    }


imputationCell : ColumnMetadata -> Grid.HtmlDetails Msg
imputationCell column =
    if column.dataType == Text then
        naDropdown
    else
        Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected [ class "form-control" ] enumImputationStrategy ImputationSelectionChanged column.imputation ]


statsColumn : StatsDict -> ColumnDisplayProperties
statsColumn stats =
    { name = "Stats"
    , viewData = statsCell stats
    , sorter = Grid.unsortable
    , headAttributes = [ class "per25" ]
    , headHtml = []
    }


statsCell : StatsDict -> ColumnMetadata -> Grid.HtmlDetails Msg
statsCell stats column =
    if column.role == Key then
        Grid.HtmlDetails [ class "stats" ]
            [ div [ class "row m0" ]
                [ div [ class "col-sm-6 pl0 pr0" ]
                    [ text "N/A" ]
                , div [ class "col-sm-6 pl0 pr0" ]
                    [ text "N/A" ]
                ]
            ]
    else
        let
            columnStat =
                Dict.get column.name stats

            statsClass =
                columnStat
                    |> Maybe.map (\s -> s |> Remote.map (\_ -> "stats") |> Remote.withDefault "stats loading")
                    |> Maybe.withDefault "stats loading"

            columnStats =
                columnStat
                    |> Maybe.withDefault Remote.Loading
                    |> Remote.map (\s -> ( s, column.dataType ))
        in
        Grid.HtmlDetails [ class statsClass ]
            [ statsDisplay columnStats ]


statsDisplay : Remote.WebData ( ColumnStats, DataType ) -> Html Msg
statsDisplay columnStats =
    case columnStats of
        Remote.Loading ->
            div [ class "row m0" ]
                [ div [ class "col-sm-6 pl0 pr0" ]
                    [ i [ class "fa fa-refresh fa-spin fa-fw" ] []
                    , span [ class "sr-only" ] [ text "Calculating..." ]
                    ]
                , div [ class "col-sm-6 pl0 pr0" ]
                    [ i [ class "fa fa-refresh fa-spin fa-fw" ] []
                    , span [ class "sr-only" ] [ text "Calculating..." ]
                    ]
                ]

        Remote.Success ( stats, dataType ) ->
            let
                errorStyle =
                    if stats.errorCount == 0 then
                        ""
                    else if (toFloat stats.errorCount / toFloat stats.totalCount) * 100 < 10 then
                        "text-danger"
                    else
                        "label label-danger"

                missingStyle =
                    if stats.missingCount == 0 then
                        ""
                    else if (toFloat stats.missingCount / toFloat stats.totalCount) * 100 < 10 then
                        "text-danger"
                    else
                        "label label-danger"

                min =
                    [ strong [] [ text "Min: " ]
                    , styledNumber <| stats.min
                    ]

                max =
                    [ strong [] [ text "Max: " ]
                    , styledNumber <| stats.max
                    ]

                count =
                    [ strong [] [ text "Count: " ]
                    , styledNumber <| commaFormatInteger stats.totalCount
                    ]

                missing =
                    [ span [ class missingStyle ]
                        [ strong [] [ text "# Missing: " ]
                        , styledNumber <| commaFormatInteger stats.missingCount
                        ]
                    ]

                mean =
                    [ strong [] [ text "Mean: " ]
                    , styledNumber <| formatFloatToString stats.mean
                    ]

                mode =
                    [ strong [] [ text "Mode: " ]
                    , styledNumber <| stats.mode
                    ]

                errors =
                    [ span [ class errorStyle ]
                        [ strong [] [ text "Errors: " ]
                        , styledNumber <| commaFormatInteger stats.errorCount
                        ]
                    ]

                stdDev =
                    [ strong [] [ text "Std Dev: " ]
                    , styledNumber <| formatFloatToString stats.stddev
                    ]

                variance =
                    [ strong [] [ text "Variance: " ]
                    , styledNumber <| formatFloatToString stats.variance
                    ]

                ( statsLeft, statsRight ) =
                    case dataType of
                        String ->
                            [ min, max, count ] => [ missing, mean, mode ]

                        Text ->
                            [ count, missing ] => []

                        Logical ->
                            [ min, max, mode ] => [ count, errors, missing ]

                        Date ->
                            [ min, max, mode ] => [ count, errors, missing ]

                        Numeric ->
                            [ min, max, stdDev, errors, variance ] => [ count, missing, mean, mode ]

                        Measure ->
                            [ min, max, stdDev, errors, variance ] => [ count, missing, mean, mode ]
            in
            div [ class "row m0" ]
                [ div [ class "col-sm-6 pl0 pr0" ]
                    (statsLeft
                        |> List.intersperse [ br [] [] ]
                        |> List.concat
                    )
                , div [ class "col-sm-6 pl0 pr0" ]
                    (statsRight
                        |> List.intersperse [ br [] [] ]
                        |> List.concat
                    )
                ]

        _ ->
            div [ class "row m0" ]
                [ div [ class "col-sm-6 pl0 pr0" ] []
                , div [ class "col-sm-6 pl0 pr0" ] []
                ]


histogramColumn : StatsDict -> ColumnDisplayProperties
histogramColumn stats =
    { name = "Distribution"
    , viewData = histogram stats
    , sorter = Grid.unsortable
    , headAttributes = [ class "per15" ]
    , headHtml = []
    }


histogram : StatsDict -> ColumnMetadata -> Grid.HtmlDetails Msg
histogram stats column =
    if column.role == Key then
        Grid.HtmlDetails [ class "stats" ]
            [ text "N/A" ]
    else
        let
            columnStats =
                stats
                    |> Dict.get column.name
                    |> Maybe.withDefault Remote.Loading
        in
        case columnStats of
            Remote.Loading ->
                Grid.HtmlDetails [ class "loading" ]
                    [ i [ class "fa fa-refresh fa-spin fa-fw" ] []
                    , span [ class "sr-only" ] [ text "Calculating..." ]
                    ]

            Remote.Success stats ->
                if column.dataType == Text then
                    Grid.HtmlDetails [ class "distribution text" ]
                        [ stats.distribution |> wordOccurrenceTable ]
                else
                    Grid.HtmlDetails []
                        [ stats.distribution |> distributionHistogram ]

            _ ->
                Grid.HtmlDetails [] [ div [] [] ]
