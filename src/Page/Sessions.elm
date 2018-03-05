module Page.Sessions exposing (Model, Msg, init, update, view, SessionColumns, defaultColumns)

import AppRoutes
import Data.Config exposing (Config)
import Data.DataSet exposing (toDataSetName)
import Data.Session exposing (..)
import Data.Status exposing (Status(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Helpers exposing (..)
import RemoteData as Remote
import Request.Session exposing (get)
import Table
import Util exposing ((=>), spinner)
import View.DeleteDialog as DeleteDialog
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager


---- MODEL ----


type alias Model =
    { sessionList : Remote.WebData SessionList
    , tableState : Table.State
    , config : Config
    , pageSize : Int
    , currentPage : Int
    , deleteDialogModel : Maybe DeleteDialog.Model
    }


defaultColumns : Dict String String -> SessionColumns msg
defaultColumns tooltips =
    SessionColumns nameColumn
        resultsActionColumn
        statusColumn
        dataSourceColumn
        typeColumn
        createdColumn


type alias SessionColumns msg =
    { name : Grid.Column SessionData msg
    , actions : Grid.Column SessionData msg
    , status : Grid.Column SessionData msg
    , dataSource : Grid.Column SessionData msg
    , sessionType : Grid.Column SessionData msg
    , created : Grid.Column SessionData msg
    }


loadSessionList : Config -> Int -> Int -> Cmd Msg
loadSessionList config pageNo pageSize =
    Request.Session.get config pageNo pageSize
        |> Remote.sendRequest
        |> Cmd.map SessionListResponse


init : Config -> ( Model, Cmd Msg )
init config =
    Model Remote.Loading (Table.initialSort "name") config 10 0 Nothing
        => loadSessionList config 0 10



-- UPDATE --


type Msg
    = SessionListResponse (Remote.WebData SessionList)
    | SetTableState Table.State
    | ChangePage Int
    | ChangePageSize Int
    | ShowDeleteDialog SessionData
    | DeleteDialogMsg DeleteDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionListResponse resp ->
            { model | sessionList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState }
                => Cmd.none

        ChangePage pgNum ->
            { model | sessionList = Remote.Loading, currentPage = pgNum }
                => loadSessionList model.config pgNum model.pageSize

        ChangePageSize pageSize ->
            { model | pageSize = pageSize, currentPage = 0 }
                => loadSessionList model.config 0 pageSize

        ShowDeleteDialog sessionData ->
            { model | deleteDialogModel = Just (DeleteDialog.init sessionData.name sessionData.sessionId) }
                => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                ignoreCascadeParams cmd _ =
                    cmd

                pendingDeleteCmd =
                    Request.Session.delete model.config >> ignoreCascadeParams

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            loadSessionList model.config model.currentPage model.pageSize
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]



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
                        [ PageSize.view ChangePageSize ]
                    ]
                , div []
                    [ Grid.view .items (config model.config.toolTips) model.tableState model.sessionList
                    , hr [] []
                    , div [ class "center" ]
                        [ Pager.view model.sessionList ChangePage ]
                    ]
                ]
            ]
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete Session"
            , bodyMessage = Just "This action cannot be undone but you can always run another session with the same parameters."
            , associatedAssets = []
            }
            |> Html.map DeleteDialogMsg
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


nameColumn : Grid.Column SessionData msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Name"
        , viewData = sessionNameCell
        , sorter = Table.increasingOrDecreasingBy .name
        , headAttributes = [ class "left per30" ]
        , headHtml = []
        }


sessionNameCell : SessionData -> Table.HtmlDetails msg
sessionNameCell model =
    Table.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.SessionDetail model.sessionId) ] [ text model.name ] ]


resultsActionColumn : Grid.Column SessionData msg
resultsActionColumn =
    Grid.veryCustomColumn
        { name = ""
        , viewData = resultsActionButton
        , sorter = Table.unsortable
        , headAttributes = []
        , headHtml = []
        }


resultsActionButton : SessionData -> Table.HtmlDetails msg
resultsActionButton model =
    Table.HtmlDetails [ class "action" ]
        --todo - make action buttons to something
        [ button [ class "btn btn-sm" ] [ text "View Results" ] ]


statusColumn : Grid.Column SessionData msg
statusColumn =
    let
        --tableDetails : SessionData -> (SessionData -> Html SessionData) -> Table.HtmlDetails Msg
        tableDetails session =
            Table.HtmlDetails []
                [ statusDisplay session.status ]
    in
    Grid.veryCustomColumn
        { name = "Status"
        , viewData = tableDetails
        , sorter = Table.increasingOrDecreasingBy (\n -> toString n.status)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


dataSourceColumn : Grid.Column SessionData msg
dataSourceColumn =
    Grid.veryCustomColumn
        { name = "Source"
        , viewData = dataSourceCell
        , sorter = Table.increasingOrDecreasingBy .dataSourceName
        , headAttributes = [ class "left per25" ]
        , headHtml = []
        }


dataSourceCell : SessionData -> Table.HtmlDetails msg
dataSourceCell model =
    Table.HtmlDetails [ class "left" ]
        [ a [ AppRoutes.href (AppRoutes.DataSetDetail (toDataSetName model.dataSourceName)) ] [ text model.dataSourceName ]
        ]


typeColumn : Grid.Column SessionData msg
typeColumn =
    Grid.veryCustomColumn
        { name = "Type"
        , viewData = typeCell
        , sorter = Table.decreasingOrIncreasingBy (\a -> toString a.predictionDomain)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


typeCell : SessionData -> Table.HtmlDetails msg
typeCell model =
    Table.HtmlDetails []
        [ text (toString model.predictionDomain)
        ]


createdColumn : Grid.Column SessionData msg
createdColumn =
    Grid.veryCustomColumn
        { name = "Created"
        , viewData = createdCell
        , sorter = Table.decreasingOrIncreasingBy .requestedDate
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


createdCell : SessionData -> Table.HtmlDetails msg
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
