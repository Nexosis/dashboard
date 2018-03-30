module View.ColumnMetadataEditor exposing (ExternalMsg(..), Model, Msg, init, subscriptions, update, updateDataSetResponse, view, viewTargetAndKeyColumns)

import Autocomplete
import Char
import Data.Columns as Columns exposing (ColumnMetadata, DataType(..), Role(..), enumDataType, enumRole)
import Data.Context exposing (ContextModel)
import Data.DataSet as DataSet exposing (ColumnStats, ColumnStatsDict, DataSetData, DataSetName, DataSetStats, toDataSetName)
import Data.ImputationStrategy exposing (ImputationStrategy(..), enumImputationStrategy)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onFocus, onInput)
import Json.Decode as Decode
import Json.Encode exposing (encode)
import Ports
import RemoteData as Remote
import Request.DataSet
import Request.Log exposing (logHttpError)
import Request.Sorting exposing (SortDirection(..), SortParameters)
import SelectWithStyle as UnionSelect
import StateStorage exposing (saveAppState)
import String.Extra as String
import Util exposing ((=>), commaFormatInteger, formatDisplayName, formatFloatToString, isJust, spinner, styledNumber)
import VegaLite exposing (Spec, combineSpecs)
import View.Charts exposing (distributionHistogram)
import View.Error exposing (viewRemoteError)
import View.Extra exposing (viewIf)
import View.Grid as Grid exposing (defaultCustomizations)
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


type alias Model =
    { columnMetadata : Remote.WebData ColumnMetadataListing
    , statsResponse : Remote.WebData DataSetStats
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


init : DataSetName -> Bool -> ( Model, Cmd Msg )
init dataSetName showTarget =
    Model Remote.Loading Remote.Loading dataSetName (Grid.initialSort "columnName" Ascending) Dict.empty Dict.empty Autocomplete.empty "" Nothing False showTarget Nothing Remote.NotAsked
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
        => (Request.DataSet.getStats context.config model.dataSetName
                |> Remote.sendRequest
                |> Cmd.map StatsResponse
           )


update : Msg -> Model -> ContextModel -> (List ColumnMetadata -> Cmd (Remote.WebData ())) -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model context pendingSaveCommand =
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
            { model | columnInEditMode = Just column } => Cmd.none => NoOp

        CancelColumnEdit ->
            { model | columnInEditMode = Nothing, changesPendingSave = Dict.empty } => Cmd.none => NoOp

        NoOpMsg ->
            model => Cmd.none => NoOp

        SaveMetadata result ->
            if Remote.isSuccess result then
                { model | modifiedMetadata = model.changesPendingSave, changesPendingSave = Dict.empty, saveResult = result, columnInEditMode = Nothing, showAutocomplete = False, previewTarget = Nothing }
                    => Ports.highlightIds (model.changesPendingSave |> Dict.keys |> List.map (\c -> "column_" ++ c |> String.classify))
                    => Updated (Dict.values model.modifiedMetadata)
            else
                { model | saveResult = result } => Cmd.none => NoOp


updateMetadata : Model -> Maybe ColumnMetadata -> (List ColumnMetadata -> Cmd (Remote.WebData ())) -> ( ( Model, Cmd Msg ), ExternalMsg )
updateMetadata model updatedColumn saveCommand =
    let
        existingColumns =
            model.columnMetadata
                |> Remote.map .metadata
                |> Remote.withDefault Dict.empty

        ensureChanged column =
            existingColumns
                |> Dict.values
                |> List.member column
                |> (\same ->
                        if same then
                            Nothing
                        else
                            Just column
                   )

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
                |> Maybe.andThen ensureChanged
                |> Maybe.andThen ensureSingleTarget
                |> Maybe.withDefault []
                |> Dict.fromListBy .name
                |> flip Dict.intersect existingColumns

        -- Keep new changes, or changes to something that has already been modified.
        -- Throw out modified columns, as these have been set back to the values we had originally.
        modifiedMetadata =
            Dict.merge (\_ changed a -> changed :: a)
                (\_ changed modified a -> changed :: a)
                (\_ modified a -> a)
                changedMetadata
                model.modifiedMetadata
                []
                |> Dict.fromListBy .name

        targetName =
            existingColumns
                |> Dict.union modifiedMetadata
                |> Dict.find (\_ c -> c.role == Target)
                |> Maybe.map (\( _, c ) -> c.name)
                |> Maybe.withDefault ""
    in
    if modifiedMetadata /= Dict.empty then
        { model | changesPendingSave = modifiedMetadata, targetQuery = targetName, saveResult = Remote.Loading } => Cmd.map SaveMetadata (saveCommand <| Dict.values modifiedMetadata) => NoOp
    else
        { model | modifiedMetadata = modifiedMetadata, columnInEditMode = Nothing, targetQuery = targetName, showAutocomplete = False, previewTarget = Nothing } => Cmd.none => NoOp


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
        stats =
            model.statsResponse
                |> Remote.map (\sr -> sr.columns)

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
            buildEditTable context stats model
    in
    div []
        [ div [ class "row mb25" ]
            [ div [ class "col-sm-3" ]
                [ h3 [] [ text "Columns" ] ]
            , div [ class "col-sm-2 col-sm-offset-7 right" ]
                [ PageSize.view ChangePageSize context.userPageSize ]
            ]
        , Grid.view filterColumnsToDisplay (config context.config.toolTips stats editTable model.columnInEditMode) model.tableState mergedMetadata
        , div [ class "center" ] [ Pager.view model.columnMetadata ChangePage ]
        ]


buildEditTable : ContextModel -> Remote.WebData ColumnStatsDict -> Model -> ColumnMetadata -> Html Msg
buildEditTable context stats model column =
    let
        saveButton =
            case model.saveResult of
                Remote.Loading ->
                    button [ class "btn btn-danger btn-sm" ] [ spinner ]

                _ ->
                    button [ class "btn btn-danger btn-sm", onClick CommitMetadataChange ] [ text "Save Changes" ]

        columnInEdit =
            model.changesPendingSave
                |> Dict.get column.name
                |> Maybe.withDefault column
    in
    tr [ class "modal fade in", style [ ( "display", "table-row" ), ( "position", "static" ) ] ]
        [ td [ class "p0", colspan 6 ]
            [ div [ class "modal-dialog modal-content metadata-editor m0", style [ ( "z-index", "1050" ), ( "width", "auto" ) ] ]
                [ Grid.view identity (editConfig context.config.toolTips stats) Grid.initialUnsorted (Remote.succeed [ columnInEdit ])
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
                , viewRemoteError model.saveResult
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


config : Dict String String -> Remote.WebData ColumnStatsDict -> (ColumnMetadata -> Html Msg) -> Maybe ColumnMetadata -> Grid.Config ColumnMetadata Msg
config toolTips stats buildEditTable columnInEdit =
    let
        makeIcon =
            helpIcon toolTips

        columns =
            nameColumn
                :: ([ ( typeColumn makeIcon, \c -> c.dataType |> toString )
                    , ( roleColumn makeIcon, \c -> c.role |> toString )
                    , ( imputationColumn makeIcon, \c -> c.imputation |> toString )
                    ]
                        |> List.map (\( c, f ) -> { c | viewData = lockedDropdownCell f })
                   )
                ++ [ statsColumn stats
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


editConfig : Dict String String -> Remote.WebData ColumnStatsDict -> Grid.Config ColumnMetadata Msg
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


customRowAttributes : ColumnMetadata -> List (Attribute Msg)
customRowAttributes column =
    if column.role == Key then
        [ id "key" ]
    else
        [ id <| String.classify <| "column_" ++ column.name, onClick <| SelectColumnForEdit column ]


nameColumn :
    { headAttributes : List (Attribute Msg)
    , headHtml : List a
    , name : String
    , sorter : Grid.Sorter { b | name : comparable }
    , viewData : { c | name : String } -> Grid.HtmlDetails Msg
    }
nameColumn =
    { name = "Column Name"
    , viewData = \c -> Grid.HtmlDetails [ class "name" ] [ text <| formatDisplayName c.name ]
    , sorter = Grid.increasingOrDecreasingBy (\c -> c.name)
    , headAttributes = [ class "left per25" ]
    , headHtml = []
    }


typeColumn :
    (String -> List (Html msg))
    ->
        { headAttributes : List (Attribute msg1)
        , headHtml : List (Html msg)
        , name : String
        , sorter : Grid.Sorter data
        , viewData : ColumnMetadata -> Grid.HtmlDetails Msg
        }
typeColumn makeIcon =
    { name = "Type"
    , viewData = dataTypeCell
    , sorter = Grid.unsortable
    , headAttributes = [ class "per10" ]
    , headHtml = text "Type " :: makeIcon "Type"
    }


dataTypeCell : ColumnMetadata -> Grid.HtmlDetails Msg
dataTypeCell column =
    Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumDataType TypeSelectionChanged column.dataType ]


emptyDropdown : Grid.HtmlDetails Msg
emptyDropdown =
    Grid.HtmlDetails [ class "form-group" ] [ select [ disabled True, class "form-control" ] [] ]


roleColumn :
    (String -> List (Html msg))
    ->
        { headAttributes : List (Attribute msg1)
        , headHtml : List (Html msg)
        , name : String
        , sorter : Grid.Sorter data
        , viewData : ColumnMetadata -> Grid.HtmlDetails Msg
        }
roleColumn makeIcon =
    { name = "Role"
    , viewData = roleCell
    , sorter = Grid.unsortable
    , headAttributes = [ class "per10" ]
    , headHtml = text "Role " :: makeIcon "Role"
    }


roleCell : ColumnMetadata -> Grid.HtmlDetails Msg
roleCell column =
    Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumRole RoleSelectionChanged column.role ]


imputationColumn :
    (String -> List (Html msg))
    ->
        { headAttributes : List (Attribute msg1)
        , headHtml : List (Html msg)
        , name : String
        , sorter : Grid.Sorter data
        , viewData : ColumnMetadata -> Grid.HtmlDetails Msg
        }
imputationColumn makeIcon =
    { name = "Imputation"
    , viewData = imputationCell
    , sorter = Grid.unsortable
    , headAttributes = [ class "per10" ]
    , headHtml = text "Imputation " :: makeIcon "Imputation"
    }


imputationCell : ColumnMetadata -> Grid.HtmlDetails Msg
imputationCell column =
    Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumImputationStrategy ImputationSelectionChanged column.imputation ]


statsColumn :
    Remote.WebData ColumnStatsDict
    ->
        { headAttributes : List (Attribute msg)
        , headHtml : List a
        , name : String
        , sorter : Grid.Sorter data
        , viewData : ColumnMetadata -> Grid.HtmlDetails Msg
        }
statsColumn stats =
    { name = "Stats"
    , viewData = statsCell stats
    , sorter = Grid.unsortable
    , headAttributes = [ class "per20" ]
    , headHtml = []
    }


statsClass : Remote.WebData a -> String
statsClass statsData =
    case statsData of
        Remote.Loading ->
            "stats loading"

        _ ->
            "stats"


statsCell : Remote.WebData ColumnStatsDict -> ColumnMetadata -> Grid.HtmlDetails Msg
statsCell stats column =
    let
        columnStats =
            stats
                |> Remote.map (\s -> Dict.get column.name s)
    in
    Grid.HtmlDetails [ class <| statsClass stats ]
        [ statsDisplay columnStats ]


statsDisplay : Remote.WebData (Maybe ColumnStats) -> Html Msg
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

        Remote.Success maybeStats ->
            case maybeStats of
                Just stats ->
                    div [ class "row m0" ]
                        [ div [ class "col-sm-6 pl0 pr0" ]
                            [ strong [] [ text "Min: " ]
                            , styledNumber <| stats.min
                            , br [] []
                            , strong [] [ text "Max: " ]
                            , styledNumber <| stats.max
                            , br [] []
                            , strong [] [ text "Std Dev: " ]
                            , styledNumber <| formatFloatToString stats.stddev
                            , br [] []
                            , strong [] [ text "Errors: " ]
                            , styledNumber <| commaFormatInteger stats.errorCount
                            ]
                        , div [ class "col-sm-6 pl0 pr0" ]
                            [ strong [] [ text "Value Count: " ]
                            , styledNumber <| commaFormatInteger stats.totalCount
                            , br [] []
                            , strong [] [ text "# Missing: " ]
                            , styledNumber <| commaFormatInteger stats.missingCount
                            , br [] []
                            , strong [] [ text "Mean: " ]
                            , styledNumber <| formatFloatToString stats.mean
                            , br [] []
                            , strong [] [ text "Median: " ]
                            , styledNumber <| formatFloatToString stats.median
                            , br [] []
                            , strong [] [ text "Mode: " ]
                            , styledNumber <| stats.mode
                            ]
                        ]

                Nothing ->
                    div [ class "row m0" ]
                        [ div [ class "col-sm-6 pl0 pr0" ] []
                        , div [ class "col-sm-6 pl0 pr0" ] []
                        ]

        _ ->
            div [ class "row m0" ]
                [ div [ class "col-sm-6 pl0 pr0" ] []
                , div [ class "col-sm-6 pl0 pr0" ] []
                ]


histogramColumn :
    Remote.WebData ColumnStatsDict
    ->
        { headAttributes : List (Attribute msg)
        , headHtml : List a
        , name : String
        , sorter : Grid.Sorter data
        , viewData : ColumnMetadata -> Grid.HtmlDetails Msg
        }
histogramColumn stats =
    { name = "Distribution"
    , viewData = histogram stats
    , sorter = Grid.unsortable
    , headAttributes = [ class "per10" ]
    , headHtml = []
    }


histogram : Remote.WebData ColumnStatsDict -> ColumnMetadata -> Grid.HtmlDetails Msg
histogram stats column =
    let
        columnStats =
            stats
                |> Remote.map (\s -> Dict.get column.name s)
    in
    case columnStats of
        Remote.Loading ->
            Grid.HtmlDetails [ class "stats loading" ]
                [ i [ class "fa fa-refresh fa-spin fa-fw" ] []
                , span [ class "sr-only" ] [ text "Calculating..." ]
                ]

        Remote.Success maybeStats ->
            case maybeStats of
                Just stats ->
                    Grid.HtmlDetails [ class "stats" ]
                        [ node "vega-chart" [ attribute "spec" (stats.distribution |> distributionHistogram |> encode 0) ] [] ]

                Nothing ->
                    Grid.HtmlDetails [] [ div [] [] ]

        _ ->
            Grid.HtmlDetails [] [ div [] [] ]
