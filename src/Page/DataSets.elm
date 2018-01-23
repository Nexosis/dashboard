module Page.DataSets exposing (view, update, Model, Msg, init)

import Html exposing (..)
import RemoteData as Remote
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSetList, DataSet)
import Request.DataSet
import Http
import Task
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , errors : List String
    , dataSetList : Remote.WebData DataSetList
    }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        loadDataSetList =
            Request.DataSet.get config
                |> Remote.sendRequest
                |> Cmd.map DataSetListResponse
    in
        (Model "DataSets" "This is the list of DataSets" [] Remote.Loading)
            => loadDataSetList



-- UPDATE --


type Msg
    = Todo
    | DataSetListResponse (Remote.WebData DataSetList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            model => Cmd.none

        DataSetListResponse resp ->
            { model | dataSetList = resp } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pageTitle ]
        , div [] [ text model.pageBody ]
        ]
