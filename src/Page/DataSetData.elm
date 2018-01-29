module Page.DataSetData exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Data.DataSet exposing (Data, DataSet, DataSetColumnsMetadata, DataSetData, DataSetName, dataSetNameDecoder, dataSetNameToString)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import RemoteData as Remote
import Request.DataSet
import Table exposing (defaultCustomizations)
import Task
import Util exposing ((=>))
import View.Pager as Pager


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetList : Remote.WebData DataSetData
    , tableState : Table.State
    , config : Config
    }


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    let
        loadDataSetList =
            Request.DataSet.getRetrieveData config dataSetName
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



-- | ChangePage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataSetDataResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        DeleteDataSet dataSet ->
            model => Cmd.none



-- ChangePage pgNum ->
--     { model | dataSetList = Remote.Loading }
--         => (Request.DataSet.getRetrieveData model.config ""
--                 |> Remote.sendRequest
--                 |> Cmd.map DataSetDataResponse
--            )
-- VIEW --


view : Model -> Html Msg
view model =
    let
        gridView =
            case model.dataSetList of
                Remote.Success dsList ->
                    gridSection dsList model.tableState

                _ ->
                    loadingGrid
    in
    div []
        [ h2 [] [ text model.pageTitle ]
        , div [] [ text model.pageBody ]
        , div [ class "row" ]
            [ div [ class "col-lg-9" ]
                [ div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ]
                        [ h3 [] [ text "Datasets" ]
                        ]
                    , div [] gridView
                    ]
                ]
            ]
        ]


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


gridSection : DataSetData -> Table.State -> List (Html Msg)
gridSection dataSetData tableState =
    [ div [ class "panel-body" ]
        [ div [ class "table-responsive" ]
            [ Table.view (config dataSetData.columns) tableState dataSetData.data ]
        ]
    , div [ class "panel-footer" ]
        []

    -- [ Pager.view dataSetData ChangePage ]
    ]


config : List DataSetColumnsMetadata -> Table.Config (Dict.Dict String String) Msg
config columns =
    Table.customConfig
        { toId = \a -> ""
        , toMsg = SetTableState
        , columns =
            columns
                |> List.map (\c -> Table.stringColumn c.name (\r -> Dict.get c.name r |> Maybe.withDefault ""))
        , customizations =
            { defaultCustomizations
                | tableAttrs = toTableAttrs
            }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ class "table table-striped" ]
