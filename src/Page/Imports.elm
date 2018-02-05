module Page.Imports exposing (Model, Msg, init, update, view)

import Data.Config exposing (Config)
import Html exposing (..)
import Request.Log exposing (Level(..), LogMessage)
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    }


init : Config -> ( Model, Cmd Msg )
init config =
    let
        message =
            LogMessage "Imports don't exist yet" Warning
    in
    Model "Imports" "This is the list of Imports"
        => Request.Log.logMessage message



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
