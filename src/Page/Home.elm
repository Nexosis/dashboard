module Page.Home exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString, toDataSetName)
import Data.Session exposing (SessionData, SessionList)
import Html exposing (..)
import Html.Attributes exposing (class)
import Page.DataSets as DataSets exposing (viewDataSetGridReadonly)
import Page.Sessions as Sessions exposing (viewSessionsGridReadonly)
import RemoteData as Remote
import Request.DataSet
import Request.Session
import Table
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { dataSetList : Remote.WebData DataSetList
    , sessionList : Remote.WebData SessionList
    , config : Config
    }


init : Config -> ( Model, Cmd Msg )
init config =
    Model
        Remote.Loading
        Remote.Loading
        config
        => Cmd.batch
            [ Request.DataSet.get config 0 5
                |> Remote.sendRequest
                |> Cmd.map DataSetListResponse
            , Request.Session.get config 0 5
                |> Remote.sendRequest
                |> Cmd.map SessionListResponse
            ]



-- UPDATE --


type Msg
    = None
    | DataSetListResponse (Remote.WebData DataSetList)
    | SessionListResponse (Remote.WebData SessionList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none

        SessionListResponse resp ->
            { model | sessionList = resp } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "API Dashboard" ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12 col-md-8 col-g-9 col-xl-9" ]
                [ viewRecentPanel "Dataset" (dataSetListView model) (AppRoutes.DataSets => Just AppRoutes.DataSetAdd)
                , viewRecentPanel "Session" (sessionListView model) (AppRoutes.Sessions => Nothing)
                , viewRecentPanel "Model" (div [] []) (AppRoutes.Models => Nothing)
                ]
            ]
        ]


dataSetListView : Model -> Html Msg
dataSetListView model =
    viewDataSetGridReadonly model.config.toolTips (Table.initialSort "dataSetName") model.dataSetList |> Html.map (\_ -> None)


sessionListView : Model -> Html Msg
sessionListView model =
    viewSessionsGridReadonly model.config.toolTips (Table.initialSort "name") model.sessionList |> Html.map (\_ -> None)


viewRecentPanel : String -> Html Msg -> ( AppRoutes.Route, Maybe AppRoutes.Route ) -> Html Msg
viewRecentPanel thing view ( linkRoute, addRoute ) =
    let
        addButton addRoute =
            case addRoute of
                Nothing ->
                    div [] []

                Just route ->
                    a [ AppRoutes.href route, class "btn btn-sm" ]
                        [ i [ class "fa fa-plus" ] []
                        , text (" Add " ++ String.toLower thing)
                        ]
    in
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-6 pl10" ]
                    [ h4 [] [ strong [] [ text ("Recent " ++ thing ++ "s") ] ]
                    ]
                , div [ class "col-sm-6 right" ]
                    [ a [ AppRoutes.href linkRoute, class "btn secondary btn-sm mr10" ] [ text ("View All " ++ thing ++ "s") ]
                    , addButton addRoute
                    ]
                ]
            , hr [ class "mt10" ] []
            , view
            ]
        ]
