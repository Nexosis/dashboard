module Page.Models exposing (Model, Msg, init, update, view, viewModelGridReadonly)

import AppRoutes as AppRoutes
import Data.Config exposing (Config)
import Data.Context exposing (ContextModel)
import Data.DisplayDate exposing (toShortDateString, toShortDateStringOrEmpty)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Nexosis.Api.Models exposing (delete, get)
import Nexosis.Types.Models exposing (ModelData, ModelList)
import Page.Helpers exposing (explainer)
import RemoteData as Remote
import Request.Sorting exposing (SortDirection(..), SortParameters)
import StateStorage exposing (saveAppState)
import Util exposing ((=>), formatDisplayName, formatDisplayNameWithWidth, spinner)
import View.Breadcrumb as Breadcrumb
import View.DeleteDialog as DeleteDialog
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


---- MODEL ----


type alias Model =
    { modelList : Remote.WebData ModelList
    , tableState : Grid.State
    , currentPage : Int
    , pageSize : Int
    , deleteDialogModel : Maybe DeleteDialog.Model
    }


loadModelList : Config -> Int -> Int -> SortParameters -> Cmd Msg
loadModelList config page pageSize sorting =
    Nexosis.Api.Models.get config.clientConfig page pageSize sorting
        |> Remote.sendRequest
        |> Cmd.map ModelListResponse


init : ContextModel -> ( Model, Cmd Msg )
init context =
    let
        initSort =
            Grid.initialSort "createdDate" Descending
    in
    Model Remote.Loading initSort 0 context.userPageSize Nothing
        => loadModelList context.config 0 context.userPageSize initSort



-- UPDATE --


type Msg
    = ModelListResponse (Remote.WebData ModelList)
    | SetTableState Grid.State
    | ChangePage Int
    | ChangePageSize Int
    | ShowDeleteDialog ModelData
    | DeleteDialogMsg DeleteDialog.Msg


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        ModelListResponse resp ->
            { model | modelList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState, modelList = Remote.Loading }
                => loadModelList context.config model.currentPage model.pageSize newState

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
                    Nexosis.Api.Models.delete context.config >> ignoreCascadeParams

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            let
                                pageNumber =
                                    model.modelList
                                        |> Remote.map
                                            (\r ->
                                                if List.length r.items == 1 && r.pageNumber /= 0 then
                                                    r.pageNumber - 1
                                                else
                                                    r.pageNumber
                                            )
                                        |> Remote.withDefault 0
                            in
                            loadModelList context.config pageNumber model.pageSize model.tableState
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]

        ChangePage pgNum ->
            { model | modelList = Remote.Loading, currentPage = pgNum }
                => loadModelList context.config pgNum model.pageSize model.tableState

        ChangePageSize pageSize ->
            let
                newModel =
                    { model | pageSize = pageSize, currentPage = 0 }
            in
            newModel
                => Cmd.batch
                    [ loadModelList context.config 0 pageSize model.tableState
                    , StateStorage.saveAppState { context | userPageSize = pageSize }
                    ]



-- VIEW --


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ class "page-header", class "row" ]
            [ Breadcrumb.list
            , div [ class "col-sm-6" ] [ h2 [] ([ text "Models " ] ++ helpIcon context.config.toolTips "Models") ]
            , div [ class "col-sm-6 right" ] []
            ]
        , hr [] []
        , div [] (viewModelListResults context model)
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete Model"
            , bodyMessage = Just "This action cannot be undone. You will have to run another session to replace this model."
            , associatedAssets = []
            }
            |> Html.map DeleteDialogMsg
        ]


viewModelListResults : ContextModel -> Model -> List (Html Msg)
viewModelListResults context model =
    let
        modelsFound =
            case model.modelList of
                Remote.Success list ->
                    not (List.isEmpty list.items)

                _ ->
                    -- Let the grid handle loading scenario
                    True
    in
    if modelsFound then
        [ div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "row mb25" ]
                    [ div [ class "col-sm-6 col-sm-offset-3" ]
                        [ Pager.view model.modelList ChangePage ]
                    , div [ class "col-sm-2 col-sm-offset-1 right" ]
                        [ PageSize.view ChangePageSize context.userPageSize ]
                    ]
                ]
            ]
        , Grid.view .items (config context.config.toolTips) model.tableState model.modelList
        , hr [] []
        , div [ class "center" ]
            [ Pager.view model.modelList ChangePage ]
        ]
    else
        [ div [ class "help col-sm-12" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "empty_model"
                ]
            ]
        ]


config : Dict String String -> Grid.Config ModelData Msg
config toolTips =
    Grid.remoteConfig
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


viewModelGridReadonly : Dict String String -> Grid.State -> Remote.WebData ModelList -> Html Grid.ReadOnlyTableMsg
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
        { name = "modelName"
        , viewData = modelNameCell
        , sorter = Grid.decreasingOrIncreasingBy (\a -> modelOrDataSourceName a)
        , headAttributes = [ class "left" ]
        , headHtml = [ text "Name" ]
        }


modelNameCell : ModelData -> Grid.HtmlDetails msg
modelNameCell model =
    Grid.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.ModelDetail model.modelId) ] [ text (formatDisplayNameWithWidth 50 <| modelOrDataSourceName model) ]
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
        , sorter = Grid.unsortable
        , headAttributes = [ class "per15" ]
        , headHtml = []
        }


predictActionButton : ModelData -> Grid.HtmlDetails msg
predictActionButton model =
    Grid.HtmlDetails [ class "action" ]
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
        , sorter = Grid.decreasingOrIncreasingBy (\a -> toString a.predictionDomain)
        , headAttributes = [ class "per15" ]
        , headHtml = []
        }


typeCell : ModelData -> Grid.HtmlDetails msg
typeCell model =
    Grid.HtmlDetails []
        [ text (toString model.predictionDomain)
        ]


createdColumn : Grid.Column ModelData msg
createdColumn =
    Grid.veryCustomColumn
        { name = "createdDate"
        , viewData = createdCell
        , sorter = Grid.decreasingOrIncreasingBy (\a -> toShortDateString a.createdDate)
        , headAttributes = [ class "per10" ]
        , headHtml = [ text "Created" ]
        }


createdCell : ModelData -> Grid.HtmlDetails msg
createdCell model =
    Grid.HtmlDetails [ class "number" ]
        [ text (toShortDateString model.createdDate)
        ]


lastUsedColumn : Grid.Column ModelData msg
lastUsedColumn =
    Grid.veryCustomColumn
        { name = "lastUsedDate"
        , viewData = lastUsedCell
        , sorter = Grid.decreasingOrIncreasingBy (\a -> toShortDateStringOrEmpty a.lastUsedDate)
        , headAttributes = [ class "per10" ]
        , headHtml = [ text "Last used" ]
        }


lastUsedCell : ModelData -> Grid.HtmlDetails msg
lastUsedCell model =
    Grid.HtmlDetails [ class "number" ]
        [ text (toShortDateStringOrEmpty model.lastUsedDate) ]


deleteColumn : Grid.Column ModelData Msg
deleteColumn =
    Grid.veryCustomColumn
        { name = "Delete"
        , viewData = deleteButton
        , sorter = Grid.unsortable
        , headAttributes = [ class "per5" ]
        , headHtml = []
        }


deleteButton : ModelData -> Grid.HtmlDetails Msg
deleteButton model =
    Grid.HtmlDetails []
        [ a [ onClick (ShowDeleteDialog model), alt "Delete", attribute "role" "button" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]
