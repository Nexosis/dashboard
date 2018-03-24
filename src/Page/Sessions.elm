module Page.Sessions exposing (Model, Msg, init, update, view, viewSessionGridReadonly)

import AppRoutes
import Data.Config exposing (Config)
import Data.Context exposing (ContextModel)
import Data.DataSet exposing (toDataSetName)
import Data.DisplayDate exposing (toShortDateString)
import Data.Session exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Helpers exposing (..)
import RemoteData as Remote
import Request.Session exposing (get)
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
    { sessionList : Remote.WebData SessionList
    , tableState : Grid.State
    , pageSize : Int
    , currentPage : Int
    , deleteDialogModel : Maybe DeleteDialog.Model
    }


defaultColumns : Dict String String -> SessionColumns msg
defaultColumns tooltips =
    SessionColumns nameColumn
        statusColumn
        dataSourceColumn
        typeColumn
        createdColumn


type alias SessionColumns msg =
    { name : Grid.Column SessionData msg
    , status : Grid.Column SessionData msg
    , dataSource : Grid.Column SessionData msg
    , sessionType : Grid.Column SessionData msg
    , created : Grid.Column SessionData msg
    }


loadSessionList : Config -> Int -> Int -> SortParameters -> Cmd Msg
loadSessionList config pageNo pageSize sortParams =
    Request.Session.get config pageNo pageSize sortParams
        |> Remote.sendRequest
        |> Cmd.map SessionListResponse


init : ContextModel -> ( Model, Cmd Msg )
init context =
    let
        initialSort =
            Grid.initialSort "requestedDate" Descending
    in
    Model Remote.Loading initialSort context.userPageSize 0 Nothing
        => loadSessionList context.config 0 context.userPageSize initialSort



-- UPDATE --


type Msg
    = SessionListResponse (Remote.WebData SessionList)
    | SetTableState Grid.State
    | ChangePage Int
    | ChangePageSize Int
    | ShowDeleteDialog SessionData
    | DeleteDialogMsg DeleteDialog.Msg


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    case msg of
        SessionListResponse resp ->
            { model | sessionList = resp } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState, sessionList = Remote.Loading }
                => loadSessionList context.config model.currentPage model.pageSize newState

        ChangePage pgNum ->
            { model | sessionList = Remote.Loading, currentPage = pgNum }
                => loadSessionList context.config pgNum model.pageSize model.tableState

        ChangePageSize pageSize ->
            let
                newModel =
                    { model | pageSize = pageSize, currentPage = 0 }
            in
            newModel
                => Cmd.batch
                    [ loadSessionList context.config 0 pageSize model.tableState
                    , StateStorage.saveAppState { context | userPageSize = pageSize }
                    ]

        ShowDeleteDialog sessionData ->
            { model | deleteDialogModel = Just (DeleteDialog.init sessionData.name sessionData.sessionId) }
                => Cmd.none

        DeleteDialogMsg subMsg ->
            let
                ignoreCascadeParams cmd _ =
                    cmd

                pendingDeleteCmd =
                    Request.Session.delete context.config >> ignoreCascadeParams

                ( ( deleteModel, cmd ), msgFromDialog ) =
                    DeleteDialog.update model.deleteDialogModel subMsg pendingDeleteCmd

                closeCmd =
                    case msgFromDialog of
                        DeleteDialog.NoOp ->
                            Cmd.none

                        DeleteDialog.Confirmed ->
                            loadSessionList context.config model.currentPage model.pageSize model.tableState
            in
            { model | deleteDialogModel = deleteModel }
                ! [ Cmd.map DeleteDialogMsg cmd, closeCmd ]



-- VIEW --


view : Model -> ContextModel -> Html Msg
view model context =
    div []
        [ div [ id "page-header", class "row" ]
            [ Breadcrumb.list
            , div [ class "col-sm-6" ] [ h2 [] ([ text "Sessions " ] ++ helpIcon context.config.toolTips "Sessions") ]
            , div [ class "col-sm-6 right" ] []
            ]
        , div [] (viewSessionListResults context model)
        , DeleteDialog.view model.deleteDialogModel
            { headerMessage = "Delete Session"
            , bodyMessage = Just "This action cannot be undone but you can always run another session with the same parameters."
            , associatedAssets = []
            }
            |> Html.map DeleteDialogMsg
        ]


viewSessionListResults : ContextModel -> Model -> List (Html Msg)
viewSessionListResults context model =
    let
        sessionsFound =
            case model.sessionList of
                Remote.Success list ->
                    not (List.isEmpty list.items)

                _ ->
                    -- Let the grid handle loading scenario
                    True
    in
    if sessionsFound then
        [ div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ div [ class "row mb25" ]
                    [ div [ class "col-sm-6 col-sm-offset-3" ]
                        [ Pager.view model.sessionList ChangePage ]
                    , div [ class "col-sm-2 col-sm-offset-1 right" ]
                        [ PageSize.view ChangePageSize context.userPageSize ]
                    ]
                ]
            ]
        , viewSessionsGrid context model.tableState model.sessionList
        , hr [] []
        , div [ class "center" ]
            [ Pager.view model.sessionList ChangePage ]
        ]
    else
        [ div [ class "help col-sm-12" ]
            [ div [ class "alert alert-info" ]
                [ explainer context.config "empty_session"
                ]
            ]
        ]


viewSessionsGrid : ContextModel -> Grid.State -> Remote.WebData SessionList -> Html Msg
viewSessionsGrid context tableState sessionList =
    Grid.view .items (config context.config.toolTips) tableState sessionList


config : Dict String String -> Grid.Config SessionData Msg
config toolTips =
    let
        col =
            defaultColumns toolTips
    in
    Grid.remoteConfig
        { toId = \a -> a.sessionId
        , toMsg = SetTableState
        , columns =
            [ col.name
            , col.status
            , col.dataSource
            , col.sessionType
            , col.created
            , deleteColumn
            ]
        }


viewSessionGridReadonly : Dict String String -> Grid.State -> Remote.WebData SessionList -> Html Grid.ReadOnlyTableMsg
viewSessionGridReadonly toolTips tableState sessionList =
    Grid.view .items (configSessionGridReadonly toolTips) tableState sessionList


configSessionGridReadonly : Dict String String -> Grid.Config SessionData Grid.ReadOnlyTableMsg
configSessionGridReadonly toolTips =
    let
        col =
            defaultColumns toolTips
    in
    Grid.config
        { toId = \a -> a.name
        , toMsg = Grid.Readonly
        , columns =
            [ col.name |> Grid.makeUnsortable
            , col.status |> Grid.makeUnsortable
            , col.dataSource |> Grid.makeUnsortable
            , col.sessionType |> Grid.makeUnsortable
            , col.created |> Grid.makeUnsortable
            ]
        }


nameColumn : Grid.Column SessionData msg
nameColumn =
    Grid.veryCustomColumn
        { name = "Name"
        , viewData = sessionNameCell
        , sorter = Grid.increasingOrDecreasingBy .name
        , headAttributes = [ class "left" ]
        , headHtml = []
        }


sessionNameCell : SessionData -> Grid.HtmlDetails msg
sessionNameCell model =
    Grid.HtmlDetails [ class "left name" ]
        [ a [ AppRoutes.href (AppRoutes.SessionDetail model.sessionId) ] [ text <| formatDisplayNameWithWidth 40 model.name ] ]


statusColumn : Grid.Column SessionData msg
statusColumn =
    let
        --tableDetails : SessionData -> (SessionData -> Html SessionData) -> Grid.HtmlDetails Msg
        tableDetails session =
            Grid.HtmlDetails []
                [ statusDisplay session.status ]
    in
    Grid.veryCustomColumn
        { name = "Status"
        , viewData = tableDetails
        , sorter = Grid.increasingOrDecreasingBy (\n -> toString n.status)
        , headAttributes = [ class "per10" ]
        , headHtml = []
        }


dataSourceColumn : Grid.Column SessionData msg
dataSourceColumn =
    Grid.veryCustomColumn
        { name = "dataSourceName"
        , viewData = dataSourceCell
        , sorter = Grid.increasingOrDecreasingBy .dataSourceName
        , headAttributes = [ class "left per25" ]
        , headHtml = [ text "Source" ]
        }


dataSourceCell : SessionData -> Grid.HtmlDetails msg
dataSourceCell model =
    Grid.HtmlDetails [ class "left", attribute "style" "width:200px" ]
        [ a [ AppRoutes.href (AppRoutes.DataSetDetail (toDataSetName model.dataSourceName)) ] [ text <| formatDisplayName model.dataSourceName ]
        ]


typeColumn : Grid.Column SessionData msg
typeColumn =
    Grid.veryCustomColumn
        { name = "Type"
        , viewData = typeCell
        , sorter = Grid.decreasingOrIncreasingBy (\a -> toString a.predictionDomain)
        , headAttributes = [ class "per15" ]
        , headHtml = []
        }


typeCell : SessionData -> Grid.HtmlDetails msg
typeCell model =
    Grid.HtmlDetails []
        [ text (toString model.predictionDomain)
        ]


createdColumn : Grid.Column SessionData msg
createdColumn =
    Grid.veryCustomColumn
        { name = "requestedDate"
        , viewData = createdCell
        , sorter = Grid.decreasingOrIncreasingBy (\a -> toShortDateString a.requestedDate)
        , headAttributes = [ class "per10" ]
        , headHtml = [ text "Created" ]
        }


createdCell : SessionData -> Grid.HtmlDetails msg
createdCell model =
    Grid.HtmlDetails [ class "number" ]
        [ text (toShortDateString model.requestedDate)
        ]


deleteColumn : Grid.Column SessionData Msg
deleteColumn =
    Grid.veryCustomColumn
        { name = "Delete"
        , viewData = deleteButton
        , sorter = Grid.unsortable
        , headAttributes = [ class "per5" ]
        , headHtml = []
        }


deleteButton : SessionData -> Grid.HtmlDetails Msg
deleteButton model =
    Grid.HtmlDetails []
        [ a [ onClick (ShowDeleteDialog model), alt "Delete", attribute "role" "button" ] [ i [ class "fa fa-trash-o" ] [] ]
        ]
