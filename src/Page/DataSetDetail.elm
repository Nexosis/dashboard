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
                Remote.Success dataSet ->
                    gridSection dataSet model.tableState

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


config : Table.Config ColumnMetadata Msg
config =
    Table.customConfig
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Column Name" .name
            , Table.stringColumn "Type" .dataType
            , Table.stringColumn "Role" .role
            , Table.stringColumn "Imputation" .imputation
            , Table.stringColumn "Stats" (\_ -> "")
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = toTableAttrs
            }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ class "table table-striped" ]
