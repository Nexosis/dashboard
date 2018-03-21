module Page.DataSetData exposing (..)

import Data.Context exposing (ContextModel)
import Html exposing (..)
import Html.Attributes exposing (..)
import Table
import Util exposing ((=>))
import View.Breadcrumb as Breadcrumb
import View.Grid as Grid
import View.PageSize as PageSize
import View.Pager as Pager
import View.Tooltip exposing (helpIcon)


type Msg
    = DataLoaded


type alias Model =
    { tableState : Table.State
    , currentPage : Int
    , pageSize : Int
    }


init : ContextModel -> ( Model, Cmd Msg )
init context =
    Model (Table.initialSort "createdDate") 0 context.userPageSize => Cmd.none


update : Msg -> Model -> ContextModel -> ( Model, Cmd Msg )
update msg model context =
    model => Cmd.none


view : ContextModel -> Html msg
view context =
    h1 [] [ text "DataSet Data" ]
