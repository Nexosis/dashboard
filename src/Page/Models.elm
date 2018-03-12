module Page.Models exposing (Model, Msg, init, update, view, viewModelGridReadonly)

import AppRoutes as AppRoutes
import Data.Config exposing (Config)
import Data.DisplayDate exposing (toShortDateString, toShortDateStringOrEmpty)
import Data.Model exposing (ModelData, ModelList)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Page.Helpers exposing (explainer)
import RemoteData as Remote
import Request.Model exposing (delete, get)
import Table
import Util exposing ((=>), spinner)
import View.DeleteDialog as DeleteDialog
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


---- MODEL ----


type alias Model =
    { modelList : Remote.WebData ModelList
    , tableState : Table.State
    , config : Config
    , currentPage : Int
    , pageSize : Int
    , deleteDialogModel : Maybe DeleteDialog.Model
    }


loadModelList : Config -> Int -> Int -> Cmd Msg
loadModelList config page pageSize =
    Request.Model.get config page pageSize
        |> Remote.sendRequest
        |> Cmd.map ModelListResponse


init : Config -> ( Model, Cmd Msg )
init config =
    Model Remote.Loading (Table.initialSort "createdDate") config 0 10 Nothing
        => loadModelList config 0 10



-- UPDATE --


type Msg
    = ModelListResponse (Remote.WebData ModelList)
    | SetTableState Table.State
    | ChangePage Int
    | ChangePageSize Int
    | ShowDeleteDialog ModelData
    | DeleteDialogMsg DeleteDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelListResponse resp ->
            { model | modelList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        ShowDeleteDialog modelData ->
            let
                displayName =
                    "model " ++ modelData.modelId
            in
            { model | deleteDialogModel = Just (DeleteDialog.init displayName modelData.modelId) }
                => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                ignoreCascadeParams request _ =
                    request

                pendingDeleteCmd =
                    Request.Model.delete model.config >> ignoreCascadeParams

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            loadModelList model.config model.currentPage model.pageSize
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]

        ChangePage pgNum ->
            { model | modelList = Remote.Loading, currentPage = pgNum }
                => loadModelList model.config pgNum model.pageSize

        ChangePageSize pageSize ->
            { model | pageSize = pageSize, currentPage = 0 }
                => loadModelList model.config 0 pageSize



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ div [ class "page-header", class "row" ]
            [ div [ class "col-sm-12" ]
                [ p [ class "breadcrumb" ]
                    [ span []
                        [ a [ href "#" ] [ text "API Dashboard" ]
                        ]
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-6" ] [ h2 [ class "mt10" ] ([ text "Models" ] ++ helpIcon model.config.toolTips "Models") ]
            , div [ class "col-sm-6 right" ] []
            ]
        , div []
            [ hr [] []
            , div [ class "row" ]
                [ div [ class "col-sm-12" ]
                    [ div [ class "row mb25" ]
                        [ div [ class "col-sm-6" ]
                            [ explainer model.config "what_is_model"
                            ]
                        , div [ class "col-sm-2 col-sm-offset-4 right" ]
                            [ PageSize.view ChangePageSize ]
                        ]
                    ]
                ]
            , Grid.view .items (config model.config.toolTips) model.tableState model.modelList
            , hr [] []
            , div [ class "center" ]
                [ Pager.view model.modelList ChangePage ]
            ]
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete Model"
            , bodyMessage = Just "This action cannot be undone. You will have to run another session to replace this model."
            , associatedAssets = []
            }
            |> Html.map DeleteDialogMsg
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
            , deleteColumn
            ]
        }


viewModelGridReadonly : Dict String String -> Table.State -> Remote.WebData ModelList -> Html Grid.ReadOnlyTableMsg
viewModelGridReadonly toolTips tableState modelList =
    Grid.view .items (configReadonly toolTips) tableState modelList


configReadonly : Dict String String -> Grid.Config ModelData Grid.ReadOnlyTableMsg
configReadonly toolTips =
    Grid.config
        { toId = \a -> a.modelId
        , toMsg = Grid.Readonly
        , columns =
            [ nameColumn |> Grid.makeUnsortable
            , predictActionColumn |> Grid.makeUnsortable
            , typeColumn |> Grid.makeUnsortable
            , createdColumn |> Grid.makeUnsortable
            , lastUsedColumn |> Grid.makeUnsortable
            ]
        }


nameColumn : Grid.Column ModelData msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Name"
        , viewData = modelNameCell
        , sorter = Table.decreasingOrIncreasingBy (\a -> modelOrDataSourceName a)
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


modelNameCell : ModelData -> Table.HtmlDetails msg
modelNameCell model =
    Table.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.ModelDetail model.modelId) ] [ text (modelOrDataSourceName model) ]
        ]


modelOrDataSourceName : ModelData -> String
modelOrDataSourceName model =
    case model.modelName of
        Just name ->
            name

        Nothing ->
            model.dataSourceName


predictActionColumn : Grid.Column ModelData msg
predictActionColumn =
    Grid.veryCustomColumn
        { name = ""
        , viewData = predictActionButton
        , sorter = Table.unsortable
        , headAttributes = []
        , headHtml = []
        }


predictActionButton : ModelData -> Table.HtmlDetails msg
predictActionButton model =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        [ a
            [ class "btn btn-danger btn-sm", AppRoutes.href (AppRoutes.ModelDetail model.modelId) ]
            [ text "Predict" ]
        ]


typeColumn : Grid.Column ModelData msg
typeColumn =
    Grid.veryCustomColumn
        { name = "Type"
        , viewData = typeCell
        , sorter = Table.decreasingOrIncreasingBy (\a -> toString a.predictionDomain)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


typeCell : ModelData -> Table.HtmlDetails msg
typeCell model =
    Table.HtmlDetails []
        [ text (toString model.predictionDomain)
        ]


createdColumn : Grid.Column ModelData msg
createdColumn =
    Grid.veryCustomColumn
        { name = "Created"
        , viewData = createdCell
        , sorter = Table.decreasingOrIncreasingBy (\a -> toShortDateString a.createdDate)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


createdCell : ModelData -> Table.HtmlDetails msg
createdCell model =
    Table.HtmlDetails []
        [ text (toShortDateString model.createdDate)
        ]


lastUsedColumn : Grid.Column ModelData msg
lastUsedColumn =
    Grid.stringColumn "Last used" (\a -> toShortDateStringOrEmpty a.lastUsedDate)


deleteColumn : Grid.Column ModelData Msg
deleteColumn =
    Grid.veryCustomColumn
        { name = "Delete"
        , viewData = deleteButton
        , sorter = Table.unsortable
        , headAttributes = [ class "per5" ]
        , headHtml = []
        }


deleteButton : ModelData -> Table.HtmlDetails Msg
deleteButton model =
    Table.HtmlDetails []
        [ button [ onClick (ShowDeleteDialog model), alt "Delete", class "btn btn-link btn-danger" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]
