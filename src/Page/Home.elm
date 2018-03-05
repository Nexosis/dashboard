module Page.Home exposing (Model, Msg, init, update, view)

import AppRoutes
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSet, DataSetList, DataSetName, dataSetNameToString, toDataSetName)
import Html exposing (..)
import Html.Attributes exposing (class)
import Page.DataSets as DataSets exposing (DataSetColumns, viewDataSetGridReadonly)
import RemoteData as Remote
import Request.DataSet
import Table
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { pageTitle : String
    , dataSetList : Remote.WebData DataSetList
    , config : Config
    }


init : Config -> ( Model, Cmd Msg )
init config =
    Model
        "API Dashboard"
        Remote.Loading
        config
        => (Request.DataSet.get config 0 5
                |> Remote.sendRequest
                |> Cmd.map DataSetListResponse
           )



-- UPDATE --


type Msg
    = None
    | DataSetListResponse (Remote.WebData DataSetList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pageTitle ]
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12 col-md-8 col-g-9 col-xl-9" ]
                [ viewRecentPanel "Dataset" (dataSetListView model) ((,) AppRoutes.DataSets (Just AppRoutes.DataSetAdd))
                , viewRecentPanel "Session" (div [] []) ((,) AppRoutes.Sessions Nothing)
                , viewRecentPanel "Model" (div [] []) ((,) AppRoutes.Models Nothing)
                ]
            ]
        ]


dataSetListView : Model -> Html Msg
dataSetListView model =
    viewDataSetGridReadonly model.config.toolTips (Table.initialSort "dataSetName") model.dataSetList |> Html.map (\_ -> None)


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
