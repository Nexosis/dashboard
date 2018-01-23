module Page.Imports exposing (view, update, Model, Msg, init)

import Html exposing (..)
import Data.Config exposing (Config)
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    }


init : Config -> ( Model, Cmd Msg )
init config =
    Model "Imports" "This is the list of Imports"
        => Cmd.none



-- UPDATE --


type Msg
    = Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.pageTitle ]
        , div [] [ text model.pageBody ]
        ]
