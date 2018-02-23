module Page.Models exposing (Model, Msg, init, update, view)

import AppRoutes as AppRoutes
import Data.Config exposing (Config)
import Data.Model exposing (ModelData, ModelList)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import RemoteData as Remote
import Request.Log as Log
import Request.Model exposing (delete, get)
import Set exposing (Set)
import Table
import Util exposing ((=>), spinner, toShortDateString)
import View.Error exposing (viewRemoteError)
import View.Grid as Grid
import View.Modal as Modal
import View.PageSize as PageSize
import View.Pager as Pager


---- MODEL ----


type alias Model =
    { modelList : Remote.WebData ModelList
    , tableState : Table.State
    , config : Config
    , page : Int
    , pageSize : Int
    , deleteDialogSetting : Maybe String
    , deleteRequest : Remote.WebData ()
    , deleteConfirmEnabled : Bool
    , deleteCascadeOptions : Set String
    , deleteConfirmInput : String
    }


loadModelList : Config -> Int -> Int -> Cmd Msg
loadModelList config page pageSize =
    Request.Model.get config page pageSize
        |> Remote.sendRequest
        |> Cmd.map ModelListResponse


init : Config -> ( Model, Cmd Msg )
init config =
    Model Remote.Loading (Table.initialSort "createdDate") config 0 10 Nothing Remote.NotAsked False Set.empty ""
        => loadModelList config 0 10



-- UPDATE --


type Msg
    = ModelListResponse (Remote.WebData ModelList)
    | SetTableState Table.State
    | ShowDeleteDialog ModelData
    | DeleteTextBoxChanged String
    | DoDelete
    | CancelDeleteDialog
    | DeleteResponse (Remote.WebData ())
    | ChangePage Int
    | ChangePageSize Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelListResponse resp ->
            { model | modelList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        ShowDeleteDialog modelData ->
            { model | deleteDialogSetting = Just modelData.modelId } => Cmd.none

        DeleteTextBoxChanged content ->
            let
                isEnabled =
                    content == "DELETE"
            in
            { model
                | deleteConfirmInput = content
                , deleteConfirmEnabled = isEnabled
            }
                => Cmd.none

        DoDelete ->
            case model.deleteDialogSetting of
                Just modelId ->
                    model
                        => (Request.Model.delete model.config modelId
                                |> Remote.sendRequest
                                |> Cmd.map DeleteResponse
                           )

                Nothing ->
                    model => Cmd.none

        CancelDeleteDialog ->
            { model
                | deleteDialogSetting = Nothing
                , deleteConfirmInput = ""
                , deleteConfirmEnabled = False
            }
                => Cmd.none

        DeleteResponse response ->
            case response of
                Remote.Success () ->
                    { model
                        | deleteDialogSetting = Nothing
                        , deleteConfirmInput = ""
                        , deleteConfirmEnabled = False
                        , deleteRequest = Remote.NotAsked
                    }
                        => loadModelList model.config model.page model.pageSize

                Remote.Failure err ->
                    { model | deleteRequest = response }
                        => Log.logHttpError err

                _ ->
                    { model | deleteRequest = response }
                        => Cmd.none

        ChangePage pgNum ->
            { model | modelList = Remote.Loading }
                => loadModelList model.config pgNum model.pageSize

        ChangePageSize pageSize ->
            { model | pageSize = pageSize }
                => loadModelList model.config 0 pageSize



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ p [ class "breadcrumb" ]
            [ span []
                [ a [ href "#" ] [ text "Api Dashboard" ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-6" ] [ h2 [ class "mt10" ] [ text "Models" ] ]
            , div [ class "col-sm-6 right" ] []
            ]
        , div []
            [ hr [] []
            , div [ class "row" ]
                [ div [ class "col-sm-12" ]
                    [ div [ class "row mb25" ]
                        [ div [ class "col-sm-6" ]
                            [ h3 [] [ text "Model Types" ]
                            , p []
                                [ text "The type of model to build is determined by the"
                                , code [] [ text "predictionDomain" ]
                                , text "property on the request. Acceptable values are: "
                                ]
                            , ul []
                                [ li []
                                    [ code [] [ text "regression" ]
                                    , text ": Builds a regression model"
                                    ]
                                , li []
                                    [ code [] [ text "classification" ]
                                    , text ": Builds a classification model"
                                    ]
                                , li []
                                    [ code [] [ text "anomalies" ]
                                    , text ": Builds an anomaly detection model"
                                    ]
                                ]
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
        , Modal.view
            (case model.deleteDialogSetting of
                Just modelIdToDelete ->
                    Just
                        { closeMessage = CancelDeleteDialog
                        , header = Just deleteModalHeader
                        , body = Just (deleteModalBody modelIdToDelete model.deleteRequest)
                        , footer = Just (deleteModalFooter model.deleteConfirmEnabled model.deleteRequest)
                        }

                Nothing ->
                    Nothing
            )
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


nameColumn : Grid.Column ModelData Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Datasource Name"
        , viewData = modelNameCell
        , sorter = Table.increasingOrDecreasingBy .dataSourceName
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


modelNameCell : ModelData -> Table.HtmlDetails Msg
modelNameCell model =
    Table.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.ModelDetail model.modelId) ] [ text model.dataSourceName ]
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
        [ button [ class "btn btn-sm" ] [ text "Predict" ] ]


typeColumn : Grid.Column ModelData Msg
typeColumn =
    Grid.veryCustomColumn
        { name = "Type"
        , viewData = typeCell
        , sorter = Table.decreasingOrIncreasingBy (\a -> toString a.predictionDomain)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


typeCell : ModelData -> Table.HtmlDetails Msg
typeCell model =
    Table.HtmlDetails []
        [ text (toString model.predictionDomain)
        ]


createdColumn : Grid.Column ModelData Msg
createdColumn =
    Grid.veryCustomColumn
        { name = "Created"
        , viewData = createdCell
        , sorter = Table.decreasingOrIncreasingBy (\a -> toShortDateString a.createdDate)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


createdCell : ModelData -> Table.HtmlDetails Msg
createdCell model =
    Table.HtmlDetails []
        [ text (toShortDateString model.createdDate)
        ]


lastUsedColumn : Grid.Column ModelData Msg
lastUsedColumn =
    Grid.stringColumn "Last used" (\a -> "?")


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
        [ button [ onClick (ShowDeleteDialog model), alt "Delete", class "btn-link" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]


deleteModalHeader : Html Msg
deleteModalHeader =
    h4 [ class "modal-title", style [ ( "color", "#fff" ), ( "font-weight", "700" ) ] ] [ text "Delete Model" ]


deleteModalBody : String -> Remote.WebData () -> Html Msg
deleteModalBody modelId deleteRequest =
    div []
        [ h5 []
            [ text "Are you sure you want to delete model "
            , strong [] [ text modelId ]
            , text "?"
            ]
        , p [] [ text "This action cannot be undone. You will have to run another sesssion to replace this model." ]
        , p [] [ text "Type ", strong [] [ text "\"DELETE\"" ], text "and then press \"confirm\" to delete." ]
        , div [ class "row m10" ]
            [ div [ class "col-sm-4" ]
                [ div [ class "form-group" ]
                    [ input [ class "form-control", placeholder "DELETE", onInput DeleteTextBoxChanged ] []
                    ]
                ]
            ]
        , viewRemoteError deleteRequest
        ]


deleteModalFooter : Bool -> Remote.WebData () -> Html Msg
deleteModalFooter confirmEnabled deleteRequest =
    let
        deleteButton =
            case deleteRequest of
                Remote.Loading ->
                    button [ class "btn btn-primary", disabled True, onClick DoDelete ] [ spinner ]

                _ ->
                    button [ class "btn btn-primary", disabled (not confirmEnabled), onClick DoDelete ] [ text "Confirm" ]
    in
    div []
        [ button [ class "btn secondary", onClick CancelDeleteDialog ] [ text "Cancel" ]
        , deleteButton
        ]
