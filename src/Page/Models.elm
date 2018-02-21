module Page.Models exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Data.Model exposing (ModelData, ModelList)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import RemoteData as Remote
import Request.Model exposing (get)
import Table
import Util exposing ((=>),toShortDateString)
import View.Grid as Grid

---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , modelList : Remote.WebData ModelList
    , tableState : Table.State
    , config : Config
    }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        req =
            Request.Model.get config 0

        loadModelList =
            req
                |> Remote.sendRequest
                |> Cmd.map ModelListResponse
    in
    Model "Models" "This is the list of Models" Remote.Loading (Table.initialSort "createdDate") config
        => loadModelList



-- UPDATE --


type Msg
    = ModelListResponse (Remote.WebData ModelList)
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelListResponse resp ->
            { model | modelList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div [][
        p [class "breadcrumb"][
        span[][a[href "#"][text "Api Dashboard"]]
    ]
    ,div [class "row"][
        div [class "col-sm-6"][h2[class "mt10"][text "Models"]]
        ,div [class "col-sm-6 right"][]
    ]
    ,div []
        [
        hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "row mb25" ]
                    [ div [ class "col-sm-6" ]
                        [ h3 [] [ text "Model Types" ]
                        , p[][
                                text "The type of model to build is determined by the"
                                ,code[][text "predictionDomain"]
                                ,text "property on the request. Acceptable values are: "
                            ]
                        , ul[][
                            li[][
                                    code [][text "regression"]                                
                                    ,text ": Builds a regression model"
                                    ]
                            ,li[][
                                code [][text "classification"]
                                ,text ": Builds a classification model"
                            ]
                            ,li[][
                                code [][text "anomalies"]
                               ,text ": Builds an anomaly detection model"
                            ]
                        ]
                        ]
                    , div [ class "col-sm-2 col-sm-offset-4 right" ]
                        [ div [ class "form-inline mr5" ]
                            -- change items per page
                            [ label [] [ text "View" ]
                            , select [class "form-control"]
                                [ option [] [ text "10" ]
                                , option [] [ text "25" ]
                                , option [] [ text "50" ]
                                , option [] [ text "100" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            , Grid.view .items (config model.config.toolTips) model.tableState model.modelList
        ]
    ]

config : Dict String String -> Grid.Config ModelData Msg
config toolTips =
    Grid.config
        { toId = \a -> a.modelId
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , predictActionColumn
            , typeColumn
            , createdColumn
            , lastUsedColumn
            , deleteActionColumn
            ]
        }


nameColumn : Grid.Column ModelData Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Datasource Name"
        , viewData = modelNameCell
        , sorter = Table.increasingOrDecreasingBy (\a -> a.dataSourceName)
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


modelNameCell : ModelData -> Table.HtmlDetails Msg
modelNameCell model =
    Table.HtmlDetails [ class "left name" ]
        [
            a [] [ text model.dataSourceName ]
        ]

predictActionColumn : Grid.Column ModelData Msg
predictActionColumn = 
       Grid.veryCustomColumn
        { name = ""
        , viewData = predictActionButton
        , sorter = Table.unsortable
        , headAttributes = []
        , headHtml = []
        }

predictActionButton : ModelData -> Table.HtmlDetails Msg
predictActionButton model =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        --todo - Change the button text and color based on the status
        [ button [ class "btn btn-sm" ] [ text "Predict" ] ]

typeColumn : Grid.Column ModelData Msg
typeColumn = 
    Grid.veryCustomColumn {
        name = "Type"
        ,viewData = typeCell
        ,sorter = Table.decreasingOrIncreasingBy (\a -> (toString a.predictionDomain))
        ,headAttributes = [class "per10"]
        ,headHtml = []
    }

typeCell : ModelData -> Table.HtmlDetails Msg
typeCell model = 
    Table.HtmlDetails [][
        text (toString model.predictionDomain)
    ]

createdColumn : Grid.Column ModelData Msg
createdColumn = Grid.veryCustomColumn {
        name = "Created"
        ,viewData = createdCell
        ,sorter = Table.decreasingOrIncreasingBy (\a -> (toShortDateString a.createdDate))
        ,headAttributes = [class "per10"]
        ,headHtml = []
    }

createdCell : ModelData -> Table.HtmlDetails Msg
createdCell model = 
    Table.HtmlDetails[][
        text (toShortDateString model.createdDate)
    ]

lastUsedColumn : Grid.Column ModelData Msg
lastUsedColumn = Grid.stringColumn "Last used" (\a->"?")

deleteActionColumn : Grid.Column ModelData Msg
deleteActionColumn = 
       Grid.veryCustomColumn
        { name = ""
        , viewData = deleteActionButton
        , sorter = Table.unsortable
        , headAttributes = []
        , headHtml = []
        }

deleteActionButton : ModelData -> Table.HtmlDetails Msg
deleteActionButton model =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        --todo - Change the button text and color based on the status
        [ a [] [ i[class "fa fa-trash-o"][] ] ]