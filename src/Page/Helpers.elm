module Page.Helpers exposing (..)

import Data.Status exposing (..)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import RemoteData as Remote


coloredStatusButton : String -> String -> Html a
coloredStatusButton input labelType =
    span [ class ("label label-" ++ labelType) ] [ text input ]


statusDisplay : Status -> Html a
statusDisplay model =
    case model of
        Completed ->
            coloredStatusButton (toString model) "success"

        Requested ->
            coloredStatusButton "pending" "info"

        Started ->
            coloredStatusButton (toString model) "warning"

        Failed ->
            coloredStatusButton (toString model) "danger"

        Cancelled ->
            coloredStatusButton (toString model) "dark"

        CancellationPending ->
            coloredStatusButton "cancellation pending" "dark"


loadingOrView : Remote.WebData a -> (a -> Html msg) -> Html msg
loadingOrView request view =
    case request of
        Remote.Success resp ->
            view resp

        Remote.Loading ->
            div [ class "loading--line" ] []

        _ ->
            div [] []
