module Page.DataSets exposing (Model, Msg(DataSetListResponse), init, update, view)

import AppRoutes exposing (Route)
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList, dataSetNameToString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as ListX
import RemoteData as Remote
import Request.DataSet
import Table exposing (defaultCustomizations)
import Util exposing ((=>))
import View.Pager as Pager


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetList : Remote.WebData DataSetList
    , tableState : Table.State
    , config : Config
    }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        req =
            Request.DataSet.get config 0

        loadDataSetList =
            req
                |> Remote.sendRequest
                |> Cmd.map DataSetListResponse
    in
    Model "DataSets" "This is the list of DataSets" [] Remote.Loading (Table.initialSort "dataSetName") config
        => loadDataSetList



-- UPDATE --


type Msg
    = DataSetListResponse (Remote.WebData DataSetList)
    | SetTableState Table.State
    | DeleteDataSet DataSet
    | ChangePage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        DeleteDataSet dataSet ->
            model => Cmd.none

        ChangePage pgNum ->
            { model | dataSetList = Remote.Loading }
                => (Request.DataSet.get model.config pgNum
                        |> Remote.sendRequest
                        |> Cmd.map DataSetListResponse
                   )



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
        --todo - breadcrumbs ?
        [ p [ class "breadcrumb" ] [ span [] [ a [ href "#" ] [ text "API Dashboard" ] ] ]
        , div [ class "row" ]
            [ div [ class "col-sm-6" ]
                [ h2 [ class "mt10" ] [ text "Datasets" ]
                ]
            , div [ class "col-sm-6 right" ]
                [ -- todo - link somewhere
                  a [ href "#", class "btn mt10" ] [ i [ class "fa fa-plus mr5" ] [], text "Add dataset" ]
                ]
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "row mb25" ]
                    [ div [ class "col-sm-6" ]
                        [ h3 [] [ text "Dataset explainer" ]
                        ]
                    , div [ class "col-sm-2 col-sm-offset-4 right" ]
                        [ div [ class "mr5" ]
                            -- change items per page
                            [ label [] [ text "View" ]
                            , select []
                                [ option [] [ text "10" ]
                                , option [] [ text "25" ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "table-responsive" ] gridView
                ]
            ]
        ]


loadingGrid : List (Html Msg)
loadingGrid =
    [ div [ class "table table-striped" ]
        [ span [] [ text "No data found" ]
        ]
    , hr [] []
    ]


gridSection : DataSetList -> Table.State -> List (Html Msg)
gridSection dataSetList tableState =
    [ Table.view config tableState dataSetList.items
    , hr [] []
    , div [ class "center" ]
        [ Pager.view dataSetList ChangePage ]
    ]


config : Table.Config DataSet Msg
config =
    Table.customConfig
        { toId = \a -> a.dataSetName |> dataSetNameToString
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , actionsColumn
            , Table.stringColumn "Size" sizeToString
            , Table.stringColumn "Shape" (\_ -> "100 x 50")
            , Table.stringColumn "Created" (\_ -> "12/26/16")
            , Table.stringColumn "Modified" (\_ -> "12/25/17")
            , deleteColumn
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = toTableAttrs
                , thead = toTableHeadAttrs [ "left per30", "", "per10", "per15", "per10", "per10", "per5" ]
            }
        }


nameColumn : Table.Column DataSet Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = dataSetNameCell
        , sorter = Table.increasingOrDecreasingBy (\a -> a.dataSetName |> dataSetNameToString)
        }


dataSetNameCell : DataSet -> Table.HtmlDetails Msg
dataSetNameCell dataSet =
    Table.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.DataSetDetail dataSet.dataSetName) ] [ text (dataSetNameToString dataSet.dataSetName) ] ]


actionsColumn : Table.Column DataSet Msg
actionsColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = dataSetActionButton
        , sorter = Table.unsortable
        }


dataSetActionButton : DataSet -> Table.HtmlDetails Msg
dataSetActionButton dataSet =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        --todo - Change the button text and color based on the status
        [ button [ class "btn btn-sm" ] [ text "Start Session" ] ]


sizeToString : DataSet -> String
sizeToString dataSet =
    if dataSet.dataSetSize == 0 then
        "-"
    else
        toString dataSet.dataSetSize ++ "B"


deleteColumn : Table.Column DataSet Msg
deleteColumn =
    Table.veryCustomColumn
        { name = "Delete"
        , viewData = dataSetDeleteButton
        , sorter = Table.unsortable
        }


dataSetDeleteButton : DataSet -> Table.HtmlDetails Msg
dataSetDeleteButton dataSet =
    Table.HtmlDetails []
        [ button [ onClick (DeleteDataSet dataSet), alt "Delete", class "btn-link" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ class "table table-striped" ]


toTableHeadAttrs : List String -> List ( String, Table.Status, Attribute msg ) -> Table.HtmlDetails msg
toTableHeadAttrs headerClasses headers =
    let
        thList =
            headerClasses
                |> ListX.zip headers
                |> List.map (\( ( name, status, onClick ), classes ) -> ( name, status, onClick, classes ))
                |> List.map headerCell
    in
    Table.HtmlDetails [] thList


headerCell : ( String, Table.Status, Attribute msg, String ) -> Html msg
headerCell ( name, status, onClick, classes ) =
    let
        content =
            case status of
                Table.Unsortable ->
                    [ Html.text name ]

                Table.Sortable selected ->
                    [ Html.text name
                    , if selected then
                        darkGray "sort-down"
                      else
                        mediumGray "sort-down"
                    ]

                Table.Reversible Nothing ->
                    [ Html.text name
                    , mediumGray "sort"
                    ]

                Table.Reversible (Just isReversed) ->
                    [ Html.text name
                    , darkGray
                        (if isReversed then
                            "sort-up"
                         else
                            "sort-down"
                        )
                    ]
    in
    Html.th [ onClick, class classes ] content


mediumGray : String -> Html msg
mediumGray icon =
    i [ class ("fa fa-" ++ icon ++ " color-mediumGray m15") ] []


darkGray : String -> Html msg
darkGray icon =
    i [ class ("fa fa-" ++ icon ++ " color-darkGray m15") ] []
