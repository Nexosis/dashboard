module Page.Helpers exposing (..)

import Data.Config exposing (Config)
import Data.Status exposing (..)
import Dict
import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (class, id, style)
import Markdown
import RemoteData as Remote


coloredStatusButton : String -> String -> Html a
coloredStatusButton input labelType =
    span [ class ("label label-" ++ labelType) ] [ text input ]


explainer : Config -> String -> Html msg
explainer config name =
    Markdown.toHtml [] (Maybe.withDefault "" (Dict.get name config.explainerContent))


makeCollapsible : String -> Html msg -> Html msg
makeCollapsible elementId view =
    ul [ id elementId, class "collapse", style [ ( "list-style-type", "none" ) ] ]
        [ li []
            [ view ]
        ]


statusDisplay : Status -> Html a
statusDisplay model =
    case model of
        Completed ->
            coloredStatusButton (toString model) "success"

        Requested ->
            coloredStatusButton "Pending" "info"

        Started ->
            coloredStatusButton (toString model) "warning"

        Failed ->
            coloredStatusButton (toString model) "danger"

        Cancelled ->
            coloredStatusButton (toString model) "dark"

        CancellationPending ->
            coloredStatusButton "Cancellation Pending" "dark"


loadingOrView : Remote.WebData a -> (a -> Html msg) -> Html msg
loadingOrView request view =
    case request of
        Remote.Success resp ->
            view resp

        Remote.Loading ->
            div [ class "loading--line" ] []

        _ ->
            div [] []
