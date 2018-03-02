module Page.Home exposing (Model, Msg, init, update, view)

import AppRoutes
import Feature exposing (Feature(..))
import Html exposing (..)
import View.Extra exposing (viewIf)


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    }


init : Model
init =
    Model
        "Home"
        "This is the homepage"



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
        , ul []
            [ li []
                [ a [ AppRoutes.href AppRoutes.DataSets ]
                    [ text "DataSets" ]
                ]
            , li []
                [ a [ AppRoutes.href AppRoutes.Imports ]
                    [ text "Imports" ]
                ]
            , li []
                [ a [ AppRoutes.href AppRoutes.Sessions ]
                    [ text "Sessions" ]
                ]
            , li []
                [ a [ AppRoutes.href AppRoutes.Models ]
                    [ text "Models" ]
                ]
            ]
        ]
