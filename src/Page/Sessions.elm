module Page.Sessions exposing (view, update, Model, Msg, init)

import Html exposing (..)


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    }


init : Model
init =
    Model "Sessions" "This is the list of Sessions"



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
