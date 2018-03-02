module Page.Home exposing (Model, Msg, init, update, view)

import AppRoutes
import Feature exposing (Feature(..))
import Html exposing (..)
import Html.Attributes exposing (class)
import View.Extra exposing (viewIf)


---- MODEL ----


type alias Model =
    { pageTitle : String
    }


init : Model
init =
    Model
        "API Dashboard"



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
        , hr [] []
        , div [ class "row" ]
            [ div [ class "col-sm-12 col-md-8 col-g-9 col-xl-9" ]
                [ viewRecentPanel "Dataset" Nothing ((,) AppRoutes.DataSets (Just AppRoutes.DataSetAdd))
                , viewRecentPanel "Session" Nothing ((,) AppRoutes.Sessions Nothing)
                , viewRecentPanel "Model" Nothing ((,) AppRoutes.Models Nothing)
                ]
            ]
        ]


viewRecentPanel : String -> a -> ( AppRoutes.Route, Maybe AppRoutes.Route ) -> Html msg
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
            , div [] []
            ]
        ]
