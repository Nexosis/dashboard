module Page.SessionStart exposing (Model, Msg, init, update, view)

import AppRoutes exposing (Route)
import Data.Config exposing (Config)
import Data.DataSet exposing (DataSetName)
import Data.Ziplist as Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData as Remote
import Util exposing ((=>))


type alias Model =
    { config : Config
    , steps : Ziplist Step
    , canAdvance : Bool
    , dataSetName : DataSetName
    }


type Msg
    = NoOp


type Step
    = SessionName


init : Config -> DataSetName -> ( Model, Cmd Msg )
init config dataSetName =
    let
        steps =
            Ziplist.create SessionName []
    in
    Model config steps False dataSetName
        => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model => Cmd.none


view : Model -> Html Msg
view model =
    div [] [ text "start" ]
