module Page.DataSets exposing (Model, Msg(DataSetListResponse), init, update, view)

import AppRoutes exposing (Route)
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList, dataSetNameToString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import RemoteData as Remote
import Request.DataSet
import Table exposing (defaultCustomizations)
import Util exposing ((=>))
import View.Grid as Grid
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


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
    div []
        --todo - breadcrumbs ?
        [ p [ class "breadcrumb" ] [ span [] [ a [ href "#" ] [ text "API Dashboard" ] ] ]
        , div [ class "row" ]
            [ div [ class "col-sm-6" ]
                [ h2 [ class "mt10" ] [ text "Datasets" ]
                ]
            , div [ class "col-sm-6 right" ]
                [ -- todo - link somewhere
                  a [ href "#", class "btn mt10" ] [ i [ class "fa fa-plus mr5" ] [], text "Add DataSet" ]
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
                , Grid.view .items (config model.config.toolTips) model.tableState model.dataSetList
                , hr [] []
                , div [ class "center" ]
                    [ Pager.view model.dataSetList ChangePage ]
                ]
            ]
        ]


config : Dict String String -> Grid.Config DataSet Msg
config toolTips =
    Grid.config
        { toId = \a -> a.dataSetName |> dataSetNameToString
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , actionsColumn
            , Grid.customStringColumn "Size" sizeToString [ class "per10" ] []
            , Grid.customUnsortableColumn "Shape" (\_ -> "100 x 50") [ class "per15" ] (helpIcon toolTips "Shape")
            , Grid.customStringColumn "Created" (\_ -> "12/26/16") [ class "per10" ] []
            , Grid.customStringColumn "Modified" (\_ -> "12/25/17") [ class "per10" ] []
            , deleteColumn
            ]
        }


nameColumn : Grid.Column DataSet Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Name"
        , viewData = dataSetNameCell
        , sorter = Table.increasingOrDecreasingBy (\a -> a.dataSetName |> dataSetNameToString)
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


dataSetNameCell : DataSet -> Table.HtmlDetails Msg
dataSetNameCell dataSet =
    Table.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.DataSetDetail dataSet.dataSetName) ] [ text (dataSetNameToString dataSet.dataSetName) ] ]


actionsColumn : Grid.Column DataSet Msg
actionsColumn =
    Grid.veryCustomColumn
        { name = ""
        , viewData = dataSetActionButton
        , sorter = Table.unsortable
        , headAttributes = []
        , headHtml = []
        }


dataSetActionButton : DataSet -> Table.HtmlDetails Msg
dataSetActionButton dataSet =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        --todo - Change the button text and color based on the status
        [ button [ class "btn btn-sm" ] [ text "Start Session" ] ]


deleteColumn : Grid.Column DataSet Msg
deleteColumn =
    Grid.veryCustomColumn
        { name = "Delete"
        , viewData = dataSetDeleteButton
        , sorter = Table.unsortable
        , headAttributes = [ class "per5" ]
        , headHtml = []
        }


dataSetDeleteButton : DataSet -> Table.HtmlDetails Msg
dataSetDeleteButton dataSet =
    Table.HtmlDetails []
        [ button [ onClick (DeleteDataSet dataSet), alt "Delete", class "btn-link" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]


sizeToString : DataSet -> String
sizeToString dataSet =
    if dataSet.dataSetSize == 0 then
        "-"
    else
        toString dataSet.dataSetSize ++ "B"
