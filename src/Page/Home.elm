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
                [ viewRecentDataSets model
                ]
            ]
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


viewRecentPanel : String -> a -> b -> AppRoutes.Route -> AppRoutes.Route -> Html msg
viewRecentPanel thing view includeAdd linkRoute addRoute =
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-6 pl10" ]
                    [ h4 [] [ strong [] [ text ("Recent " ++ thing ++ "s") ] ]
                    ]
                , div [ class "col-sm-6 right" ]
                    [ a [ AppRoutes.href linkRoute, class "btn secondary btn-sm mr10" ] [ text ("View All " ++ thing ++ "s") ]
                    , a [ AppRoutes.href addRoute, class "btn btn-sm" ]
                        [ i [ class "fa fa-plus" ] []
                        , text (" Add " ++ String.toLower thing)
                        ]
                    ]
                ]
            , hr [ class "mt10" ] []
            , div [] []
            ]
        ]


viewRecentDataSets : a -> Html msg
viewRecentDataSets model =
    viewRecentPanel "Dataset" Nothing True AppRoutes.DataSets AppRoutes.DataSetAdd
