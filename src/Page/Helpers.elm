module Page.Helpers exposing (..)

import Data.Session exposing (SessionData)
import Data.Status exposing (..)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)


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
