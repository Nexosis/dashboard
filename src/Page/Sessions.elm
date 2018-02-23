module Page.Sessions exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Config exposing (Config)
import Data.DataSet exposing (toDataSetName)
import Data.Session exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData as Remote
import Request.Log as Log
import Request.Session exposing (get)
import Set exposing (Set)
import Table
import Util exposing ((=>), spinner, toShortDateString)
import View.Error exposing (viewRemoteError)
import View.Grid as Grid
import View.Modal as Modal
import View.Pager as Pager


---- MODEL ----


type alias Model =
    { sessionList : Remote.WebData SessionList
    , tableState : Table.State
    , config : Config
    , deleteDialogSetting : Maybe String
    , deleteRequest : Remote.WebData ()
    , deleteConfirmEnabled : Bool
    , deleteCascadeOptions : Set String
    , deleteConfirmInput : String
    }


loadSessionList : Config -> Int -> Cmd Msg
loadSessionList config pageNo =
    Request.Session.get config pageNo
        |> Remote.sendRequest
        |> Cmd.map SessionListResponse


init : Config -> ( Model, Cmd Msg )
init config =
    Model Remote.Loading (Table.initialSort "name") config Nothing Remote.NotAsked False Set.empty ""
        => loadSessionList config 0



-- UPDATE --


type Msg
    = SessionListResponse (Remote.WebData SessionList)
    | SetTableState Table.State
    | ShowDeleteDialog SessionData
    | DeleteTextBoxChanged String
    | DoDelete
    | CancelDeleteDialog
    | DeleteResponse (Remote.WebData ())
    | ChangePage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionListResponse resp ->
            { model | sessionList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        ShowDeleteDialog sessionData ->
            { model | deleteDialogSetting = Just sessionData.sessionId } => Cmd.none

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
                Just sessionId ->
                    model
                        => (Request.Session.delete model.config sessionId
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
                        => loadSessionList model.config 0

                Remote.Failure err ->
                    { model | deleteRequest = response }
                        => Log.logHttpError err

                _ ->
                    { model | deleteRequest = response }
                        => Cmd.none

        ChangePage pgNum ->
            { model | sessionList = Remote.Loading }
                => loadSessionList model.config pgNum



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
            [ div [ class "col-sm-6" ] [ h2 [ class "mt10" ] [ text "Sessions" ] ]
            , div [ class "col-sm-6 right" ] []
            ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "row mb25" ]
                    [ div [ class "col-sm-6" ] [ h3 [] [ text "Session Explainer" ] ]
                    , div [ class "col-sm-2 col-sm-offset-4 right" ]
                        [ div [ class "form-inline mr5" ]
                            -- change items per page
                            [ label [] [ text "View" ]
                            , select [ class "form-control" ]
                                [ option [] [ text "10" ]
                                , option [] [ text "25" ]
                                , option [] [ text "50" ]
                                , option [] [ text "100" ]
                                ]
                            ]
                        ]
                    ]
                , div []
                    [ Grid.view .items (config model.config.toolTips) model.tableState model.sessionList
                    , hr [] []
                    , div [ class "center" ]
                        [ Pager.view model.sessionList ChangePage ]
                    ]
                ]
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


config : Dict String String -> Grid.Config SessionData Msg
config toolTips =
    Grid.config
        { toId = \a -> a.sessionId
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , resultsActionColumn
            , statusColumn
            , dataSourceColumn
            , typeColumn
            , createdColumn
            , deleteColumn
            ]
        }


nameColumn : Grid.Column SessionData Msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Name"
        , viewData = sessionNameCell
        , sorter = Table.increasingOrDecreasingBy .name
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


sessionNameCell : SessionData -> Table.HtmlDetails Msg
sessionNameCell model =
    Table.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.SessionDetail model.sessionId) ] [ text model.name ] ]


resultsActionColumn : Grid.Column SessionData Msg
resultsActionColumn =
    Grid.veryCustomColumn
        { name = ""
        , viewData = resultsActionButton
        , sorter = Table.unsortable
        , headAttributes = []
        , headHtml = []
        }


resultsActionButton : SessionData -> Table.HtmlDetails Msg
resultsActionButton model =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        [ button [ class "btn btn-sm" ] [ text "View Results" ] ]


statusColumn : Grid.Column SessionData Msg
statusColumn =
    Grid.veryCustomColumn
        { name = "Status"
        , viewData = statusDisplay
        , sorter = Table.increasingOrDecreasingBy (\n -> toString n.status)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


statusDisplay : SessionData -> Table.HtmlDetails Msg
statusDisplay model =
    Table.HtmlDetails []
        [ case model.status of
            Completed ->
                coloredStatusButton (toString model.status) "success"

            Requested ->
                coloredStatusButton "pending" "info"

            Started ->
                coloredStatusButton (toString model.status) "warning"

            Failed ->
                coloredStatusButton (toString model.status) "danger"
        ]


coloredStatusButton : String -> String -> Html Msg
coloredStatusButton input labelType =
    span [ class ("label label-" ++ labelType) ] [ text input ]


dataSourceColumn : Grid.Column SessionData Msg
dataSourceColumn =
    Grid.veryCustomColumn
        { name = "Source"
        , viewData = dataSourceCell
        , sorter = Table.increasingOrDecreasingBy .dataSourceName
        , headAttributes = [ class "left per25" ]
        , headHtml = []
        }


dataSourceCell : SessionData -> Table.HtmlDetails Msg
dataSourceCell model =
    Table.HtmlDetails [ class "left" ]
        [ a [ AppRoutes.href (AppRoutes.DataSetDetail (toDataSetName model.dataSourceName)) ] [ text model.dataSourceName ]
        ]


typeColumn : Grid.Column SessionData Msg
typeColumn =
    Grid.veryCustomColumn
        { name = "Type"
        , viewData = typeCell
        , sorter = Table.decreasingOrIncreasingBy (\a -> toString a.predictionDomain)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


typeCell : SessionData -> Table.HtmlDetails Msg
typeCell model =
    Table.HtmlDetails []
        [ text (toString model.predictionDomain)
        ]


createdColumn : Grid.Column SessionData Msg
createdColumn =
    Grid.veryCustomColumn
        { name = "Created"
        , viewData = createdCell
        , sorter = Table.decreasingOrIncreasingBy .requestedDate
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


createdCell : SessionData -> Table.HtmlDetails Msg
createdCell model =
    Table.HtmlDetails []
        --TODO: date from SessionData model. change decoder
        [ text (String.dropRight 22 model.requestedDate)
        ]


deleteColumn : Grid.Column SessionData Msg
deleteColumn =
    Grid.veryCustomColumn
        { name = "Delete"
        , viewData = deleteButton
        , sorter = Table.unsortable
        , headAttributes = [ class "per5" ]
        , headHtml = []
        }


deleteButton : SessionData -> Table.HtmlDetails Msg
deleteButton model =
    Table.HtmlDetails []
        [ button [ onClick (ShowDeleteDialog model), alt "Delete", class "btn-link" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]


deleteModalHeader : Html Msg
deleteModalHeader =
    h4 [ class "modal-title", style [ ( "color", "#fff" ), ( "font-weight", "700" ) ] ] [ text "Delete Model" ]


deleteModalBody : String -> Remote.WebData () -> Html Msg
deleteModalBody sessionId deleteRequest =
    div []
        [ h5 []
            [ text "Are you sure you want to delete session "
            , strong [] [ text sessionId ]
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
