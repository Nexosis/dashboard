module Page.Home exposing (Model, Msg, init, update, view)

import Feature exposing (Feature(..))
import Html exposing (..)
import Route
import View.Extra exposing (viewIf)


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    , showModels : Bool
    , showImports : Bool
    , showSessions : Bool
    , showDataSets : Bool
    }


init : (Feature -> Bool) -> Model
init isEnabled =
    Model
        "Home"
        "This is the homepage"
        (isEnabled Models)
        (isEnabled Imports)
        (isEnabled Sessions)
        (isEnabled DataSets)



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
            [ viewIf
                (\() ->
                    li []
                        [ a [ Route.href (Route.DataSetRoute Route.DataSets) ]
                            [ text "DataSets" ]
                        ]
                )
                model.showDataSets
            , viewIf
                (\() ->
                    li []
                        [ a [ Route.href Route.Imports ]
                            [ text "Imports" ]
                        ]
                )
                model.showImports
            , viewIf
                (\() ->
                    li []
                        [ a [ Route.href Route.Sessions ]
                            [ text "Sessions" ]
                        ]
                )
                model.showSessions
            , viewIf
                (\() ->
                    li []
                        [ a [ Route.href Route.Models ]
                            [ text "Models" ]
                        ]
                )
                model.showModels
            ]
        ]
