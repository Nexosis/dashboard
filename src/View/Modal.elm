module View.Modal exposing (Config, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe exposing (andThen)
import Util exposing (isJust)


type alias Config msg =
    { closeMessage : msg
    , header : Maybe (Html msg)
    , body : Maybe (Html msg)
    , footer : Maybe (Html msg)
    }


map : (a -> b) -> Config a -> Config b
map f config =
    { closeMessage = f config.closeMessage
    , header = Maybe.map (Html.map f) config.header
    , body = Maybe.map (Html.map f) config.body
    , footer = Maybe.map (Html.map f) config.footer
    }


view : Maybe (Config msg) -> Html msg
view maybeConfig =
    let
        displayed =
            isJust maybeConfig
    in
    div []
        [ div
            [ classList
                [ ( "modal", True )
                , ( "fade", True )
                , ( "in", True )
                ]
            , tabindex -1
            , style
                [ ( "display"
                  , if displayed then
                        "block"
                    else
                        "none"
                  )
                ]
            ]
            [ div
                [ class "modal-dialog", style [ ( "z-index", "1050" ) ] ]
                [ div
                    [ class "modal-content" ]
                    (case maybeConfig of
                        Nothing ->
                            [ empty ]

                        Just config ->
                            [ header config.closeMessage config.header
                            , body config.body
                            , footer config.footer
                            ]
                    )
                ]
            , backdrop maybeConfig
            ]
        ]


header : msg -> Maybe (Html msg) -> Html msg
header closeMessage header =
    if header == Nothing then
        empty
    else
        div [ class "modal-header bg-color-magenta" ]
            [ closeButton closeMessage
            , Maybe.withDefault empty header
            ]


closeButton : msg -> Html msg
closeButton closeMessage =
    button [ class "close", onClick closeMessage ]
        [ span [] [ text "x" ] ]


body : Maybe (Html msg) -> Html msg
body maybeContent =
    case maybeContent of
        Nothing ->
            empty

        Just content ->
            div [ class "modal-body p15" ]
                [ content ]


footer : Maybe (Html msg) -> Html msg
footer maybeContent =
    case maybeContent of
        Nothing ->
            empty

        Just content ->
            div [ class "modal-footer p15" ]
                [ content ]


backdrop : Maybe (Config msg) -> Html msg
backdrop maybeConfig =
    let
        maybeOnClick =
            case maybeConfig of
                Nothing ->
                    []

                Just config ->
                    [ onClick config.closeMessage ]
    in
    div
        ([ classList [ ( "modal-backdrop fade in", isJust maybeConfig ) ]
         ]
            ++ maybeOnClick
        )
        []


empty : Html msg
empty =
    div [] []
