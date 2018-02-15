module Page.DataSetDetail exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Data.DataSet exposing (ColumnMetadata, DataSet, DataSetData, DataSetName)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode
import Ports
import RemoteData as Remote
import Request.DataSet
import Table exposing (defaultCustomizations)
import Util exposing ((=>))
import VegaLite exposing (Spec)
import View.Grid as Grid
import View.Tooltip exposing (helpIcon)


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetResponse : Remote.WebData DataSetData
    , tableState : Table.State
    , config : Config
    }


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    let
        loadDataSetList =
            Request.DataSet.getRetrieveDetail config dataSetName
                |> Remote.sendRequest
                |> Cmd.map DataSetDataResponse
    in
    Model "DataSets" "This is the list of DataSets" [] Remote.Loading (Table.initialSort "dataSetName") config
        => loadDataSetList



-- UPDATE --


type Msg
    = DataSetDataResponse (Remote.WebData DataSetData)
    | SetTableState Table.State
    | DeleteDataSet DataSet


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
                    in
                    { model | dataSetResponse = resp } => Ports.drawVegaChart vegaSpec

                _ ->
                    { model | dataSetResponse = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        DeleteDataSet dataSet ->
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



-- VIEW --


view : Model -> Html Msg
view model =
    let
        gridView =
            case model.dataSetResponse of
                Remote.Success dataSet ->
                    gridSection dataSet model.tableState model.config.toolTips

                Remote.Failure error ->
                    case error of
                        Http.BadStatus response ->
                            errorDisplay response

                        _ ->
                            [ div [] [] ]

                _ ->
                    loadingGrid
    in
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
                    , div [ class "table-responsive" ] gridView
                    ]
                ]
            ]
        ]


errorDisplay : Http.Response String -> List (Html Msg)
errorDisplay error =
    [ div [] [ text (error.body |> toString) ] ]


loadingGrid : List (Html Msg)
loadingGrid =
    [ div [ class "panel-body" ]
        [ div [ class "table-responsive" ]
            [ span [] [ text "No data found" ]
            ]
        ]
    , div [ class "panel-footer" ]
        []
    ]


gridSection : DataSetData -> Table.State -> Dict String String -> List (Html Msg)
gridSection dataSetData tableState toolTips =
    [ div [ class "panel-body" ]
        [ div [ class "table-responsive" ]
            [ Table.view (config toolTips) tableState dataSetData.columns ]
        ]
    , div [ class "panel-footer" ]
        []

    -- [ Pager.view dataSetData ChangePage ]
    ]


config : Dict String String -> Table.Config ColumnMetadata Msg
config toolTips =
    let
        makeIcon =
            helpIcon toolTips
    in
    Grid.config
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , typeColumn makeIcon
            , roleColumn makeIcon
            , imputationColumn makeIcon
            , Grid.customUnsortableColumn "Stats" (\_ -> "-") [ class "per20", colspan 2 ] []
            ]
        }


nameColumn : Grid.Column ColumnMetadata Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Column Name"
        , viewData = columnNameCell
        , sorter = Table.increasingOrDecreasingBy .name
        , headAttributes = [ class "left per25" ]
        , headHtml = []
        }


columnNameCell : ColumnMetadata -> Table.HtmlDetails Msg
columnNameCell metadata =
    Table.HtmlDetails [ class "name" ]
        [ text metadata.name ]


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
dataTypeCell metadata =
    Table.HtmlDetails [ class "form-group" ]
        [ select [ class "form-control" ]
            [ option [] [ text metadata.dataType ]
            ]
        ]


roleColumn : (String -> List (Html Msg)) -> Grid.Column ColumnMetadata Msg
roleColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Role"
        , viewData = dataTypeCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Role"
        }


roleCell : ColumnMetadata -> Table.HtmlDetails Msg
roleCell metadata =
    Table.HtmlDetails [ class "form-group" ]
        [ select [ class "form-control" ]
            [ option [] [ text metadata.role ]
            ]
        ]


imputationColumn : (String -> List (Html Msg)) -> Grid.Column ColumnMetadata Msg
imputationColumn makeIcon =
    Grid.veryCustomColumn
        { name = "Imputation"
        , viewData = dataTypeCell
        , sorter = Table.unsortable
        , headAttributes = [ class "per10" ]
        , headHtml = makeIcon "Imputation"
        }


imputationCell : ColumnMetadata -> Table.HtmlDetails Msg
imputationCell metadata =
    Table.HtmlDetails [ class "form-group" ]
        [ select [ class "form-control" ]
            [ option [] [ text metadata.imputation ]
            ]
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
