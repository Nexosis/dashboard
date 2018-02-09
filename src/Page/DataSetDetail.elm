module Page.DataSetDetail exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Data.DataSet exposing (ColumnMetadata, DataSet, DataSetData, DataSetName)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import RemoteData as Remote
import Request.DataSet
import Table exposing (defaultCustomizations)
import Util exposing ((=>))


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
            { model | dataSetResponse = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        DeleteDataSet dataSet ->
            model => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    let
        gridView =
            case model.dataSetResponse of
                Remote.Success dsList ->
                    gridSection dsList model.tableState

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


gridSection : DataSetData -> Table.State -> List (Html Msg)
gridSection dataSetData tableState =
    [ div [ class "panel-body" ]
        [ div [ class "table-responsive" ]
            [ Table.view config tableState dataSetData.columns ]
        ]
    , div [ class "panel-footer" ]
        []

    -- [ Pager.view dataSetData ChangePage ]
    ]



-- { dataType : String
-- , role : String
-- , imputation : String
-- , aggregation : String
-- , name : String
-- }


config : Table.Config ColumnMetadata Msg
config =
    Table.customConfig
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.stringColumn "Type" .dataType
            , Table.stringColumn "Role" .role
            , Table.stringColumn "Imputation" .imputation
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = toTableAttrs
            }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ class "table table-striped" ]
