module View.ColumnMetadataEditor exposing (ExternalMsg(..), Model, Msg, init, subscriptions, update, updateDataSetResponse, view, viewTargetAndKeyColumns)

import Autocomplete
import Char
import Data.Columns as Columns exposing (ColumnMetadata, DataType(..), Role(..), enumDataType, enumRole)
import Data.Context exposing (ContextModel)
import Data.DataSet as DataSet exposing (ColumnStats, ColumnStatsDict, DataSetData, DataSetName, DataSetStats, toDataSetName)
import Data.ImputationStrategy exposing (ImputationStrategy(..), enumImputationStrategy)
import Dict exposing (Dict)
import Dict.Extra as DictX
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onFocus, onInput)
import RemoteData as Remote
import Request.DataSet
import Request.Log exposing (logHttpError)
import Request.Sorting exposing (SortDirection(..), SortParameters)
import SelectWithStyle as UnionSelect
import StateStorage exposing (saveAppState)
import String.Extra as String
import Util exposing ((=>), commaFormatInteger, formatDisplayName, formatFloatToString, styledNumber)
import VegaLite exposing (Spec, combineSpecs)
import View.Charts exposing (distributionHistogram)
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
    , autoState : Autocomplete.State
    , targetQuery : String
    , previewTarget : Maybe String
    , showAutocomplete : Bool
    , showTarget : Bool
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
    | TypeSelectionChanged ColumnMetadata DataType
    | RoleSelectionChanged ColumnMetadata Role
    | ImputationSelectionChanged ColumnMetadata ImputationStrategy
    | SetAutoCompleteState Autocomplete.Msg
    | SetTarget String
    | SetQuery String
    | PreviewTarget String
    | AutocompleteWrap Bool
    | AutocompleteReset


init : DataSetName -> Bool -> ( Model, Cmd Msg )
init dataSetName showTarget =
    Model Remote.Loading Remote.Loading dataSetName (Grid.initialSort "columnName" Ascending) Dict.empty Autocomplete.empty "" Nothing False showTarget
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
    , metadata = DictX.fromListBy .name columns
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
                |> DictX.find (\_ m -> m.role == Columns.Target)
                |> Maybe.map Tuple.first
                |> Maybe.withDefault ""
    in
    { model | columnMetadata = mappedColumns, targetQuery = targetName, showAutocomplete = False }
        => (Request.DataSet.getStats context.config model.dataSetName
                |> Remote.sendRequest
                |> Cmd.map StatsResponse
           )


update : Msg -> Model -> ContextModel -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model context =
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

        RoleSelectionChanged metadata selection ->
            if selection == Target then
                update (SetTarget metadata.name) model context
            else
                let
                    updatedModel =
                        { model
                            | modifiedMetadata =
                                updateRole (getExistingOrOriginalColumn model.modifiedMetadata metadata) selection
                                    |> maybeAppendColumn model.modifiedMetadata
                        }
                in
                updatedModel
                    => Cmd.none
                    => Updated (Dict.values updatedModel.modifiedMetadata)

        TypeSelectionChanged metadata selection ->
            let
                modifiedMetadata =
                    updateDataType (getExistingOrOriginalColumn model.modifiedMetadata metadata) selection
                        |> maybeAppendColumn model.modifiedMetadata
            in
            { model | modifiedMetadata = modifiedMetadata }
                => Cmd.none
                => Updated (Dict.values modifiedMetadata)

        ImputationSelectionChanged metadata selection ->
            let
                modifiedMetadata =
                    updateImputation (getExistingOrOriginalColumn model.modifiedMetadata metadata) selection
                        |> maybeAppendColumn model.modifiedMetadata
            in
            { model | modifiedMetadata = modifiedMetadata }
                => Cmd.none
                => Updated (Dict.values modifiedMetadata)

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
                    update updateMsg newModel context

        PreviewTarget targetName ->
            { model | previewTarget = Just targetName } => Cmd.none => NoOp

        AutocompleteWrap toTop ->
            case model.previewTarget of
                Just target ->
                    update AutocompleteReset model context

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
                ( newTarget, oldTarget ) =
                    model.columnMetadata
                        |> Remote.map
                            (\cm ->
                                let
                                    newTarget =
                                        cm.metadata
                                            |> Dict.get targetName

                                    oldTarget =
                                        cm.metadata
                                            |> Dict.union model.modifiedMetadata
                                            |> DictX.find (\_ c -> c.role == Target)
                                            |> Maybe.map Tuple.second
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
            { model | modifiedMetadata = updatedMetadata, targetQuery = targetName, showAutocomplete = False, previewTarget = Nothing } => Cmd.none => Updated (Dict.values updatedMetadata)


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


getExistingOrOriginalColumn : Dict String ColumnMetadata -> ColumnMetadata -> ColumnMetadata
getExistingOrOriginalColumn modifiedList column =
    modifiedList
        |> Dict.get column.name
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


maybeAppendColumn : Dict String ColumnMetadata -> ( ColumnMetadata, Bool ) -> Dict String ColumnMetadata
maybeAppendColumn dict ( metadata, unchanged ) =
    if unchanged then
        dict
    else
        Dict.insert metadata.name metadata dict


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
                |> Remote.withDefault Dict.empty

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
    in
    div []
        [ div [ class "row mb25" ]
            [ div [ class "col-sm-3" ]
                [ h3 [] [ text "Columns" ] ]
            , div [ class "col-sm-2 col-sm-offset-7 right" ]
                [ PageSize.view ChangePageSize context.userPageSize ]
            ]
        , Grid.view filterColumnsToDisplay (config context.config.toolTips stats) model.tableState mergedMetadata
        , div [ class "center" ] [ Pager.view model.columnMetadata ChangePage ]
        ]


viewTargetAndKeyColumns : ContextModel -> Model -> Html Msg
viewTargetAndKeyColumns context model =
    let
        ( keyFormGroup, targetFormGroup ) =
            case model.columnMetadata of
                Remote.Success resp ->
                    let
                        keyGroup =
                            resp.metadata
                                |> DictX.find (\_ m -> m.role == Columns.Key)
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
            [ label [ class "control-label col-sm-3 mr0 pr0" ] (text "Target" :: helpIcon context.config.toolTips "Target")
            , div [ class "col-sm-8" ]
                [ input [ type_ "text", class "form-control", value queryText, onInput SetQuery ] []
                , viewIf
                    (\() ->
                        div [ class "autocomplete-menu" ] [ Html.map SetAutoCompleteState (Autocomplete.view viewConfig 5 model.autoState (filterColumnNames model.targetQuery model.columnMetadata)) ]
                    )
                    model.showAutocomplete
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


config : Dict String String -> ColumnStatsDict -> Grid.Config ColumnMetadata Msg
config toolTips stats =
    let
        makeIcon =
            helpIcon toolTips
    in
    Grid.configCustom
        { toId = \c -> c.name
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , typeColumn makeIcon
            , roleColumn makeIcon
            , imputationColumn makeIcon
            , statsColumn stats
            , histogramColumn stats
            ]
        , customizations = \defaults -> { defaults | rowAttrs = customRowAttributes }
        }


customRowAttributes : ColumnMetadata -> List (Attribute Msg)
customRowAttributes column =
    if column.role == Key then
        [ id "key" ]
    else
        []


nameColumn : Grid.Column ColumnMetadata Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Column Name"
        , viewData = columnNameCell
        , sorter = Grid.increasingOrDecreasingBy (\c -> c.name)
        , headAttributes = [ class "left per25" ]
        , headHtml = []
        }


columnNameCell : ColumnMetadata -> Grid.HtmlDetails Msg
columnNameCell column =
    Grid.HtmlDetails [ class "name" ]
        [ text <| formatDisplayName column.name ]


typeColumn : (String -> List (Html Msg)) -> Grid.Column ColumnMetadata Msg
typeColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Type"
        , viewData = dataTypeCell
        , sorter = Grid.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = text "Type " :: makeIcon "Type"
        }


dataTypeCell : ColumnMetadata -> Grid.HtmlDetails Msg
dataTypeCell column =
    if column.role == Key then
        emptyDropdown
    else
        Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumDataType (TypeSelectionChanged column) column.dataType ]


emptyDropdown : Grid.HtmlDetails Msg
emptyDropdown =
    Grid.HtmlDetails [ class "form-group" ] [ select [ disabled True, class "form-control" ] [] ]


roleColumn : (String -> List (Html Msg)) -> Grid.Column ColumnMetadata Msg
roleColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Role"
        , viewData = roleCell
        , sorter = Grid.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = text "Role " :: makeIcon "Role"
        }


roleCell : ColumnMetadata -> Grid.HtmlDetails Msg
roleCell column =
    if column.role == Key then
        Grid.HtmlDetails [ class "form-group" ] [ select [ disabled True, class "form-control" ] [ option [] [ text "Key" ] ] ]
    else
        Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumRole (RoleSelectionChanged column) column.role ]


enumOption : e -> e -> Html Msg
enumOption roleOption roleModel =
    option [ selected (roleOption == roleModel) ] [ text (toString roleOption) ]


imputationColumn : (String -> List (Html Msg)) -> Grid.Column ColumnMetadata Msg
imputationColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Imputation"
        , viewData = imputationCell
        , sorter = Grid.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = text "Imputation " :: makeIcon "Imputation"
        }


imputationCell : ColumnMetadata -> Grid.HtmlDetails Msg
imputationCell column =
    if column.role == Key then
        emptyDropdown
    else
        Grid.HtmlDetails [ class "form-group" ] [ UnionSelect.fromSelected "form-control" enumImputationStrategy (ImputationSelectionChanged column) column.imputation ]


statsColumn : ColumnStatsDict -> Grid.Column ColumnMetadata Msg
statsColumn stats =
    Grid.veryCustomColumn
        { name = "Stats"
        , viewData = statsCell stats
        , sorter = Grid.unsortable
        , headAttributes = [ class "per20" ]
        , headHtml = []
        }


statsCell : ColumnStatsDict -> ColumnMetadata -> Grid.HtmlDetails Msg
statsCell stats column =
    let
        columnStats =
            Dict.get column.name stats
    in
    Grid.HtmlDetails [ class "stats" ]
        [ statsDisplay columnStats ]


statsDisplay : Maybe ColumnStats -> Html Msg
statsDisplay columnStats =
    case columnStats of
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


histogramColumn : ColumnStatsDict -> Grid.Column ColumnMetadata Msg
histogramColumn stats =
    Grid.veryCustomColumn
        { name = "Distribution"
        , viewData = histogram stats
        , sorter = Grid.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


histogram : ColumnStatsDict -> ColumnMetadata -> Grid.HtmlDetails Msg
histogram stats column =
    let
        columnStats =
            Dict.get column.name stats
    in
    case columnStats of
        Just stats ->
            Grid.HtmlDetails []
                [ div [ id ("histogram_" ++ column.name |> String.classify) ] []
                , stats.distribution |> distributionHistogram
                ]

        Nothing ->
            Grid.HtmlDetails [] [ div [] [] ]
