module Page.Helpers exposing (..)

import Data.Config exposing (Config)
import Dict
import Html exposing (Html, div, h5, i, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, id, style)
import List
import Markdown
import Nexosis.Types.Message exposing (Message)
import Nexosis.Types.Status exposing (..)
import RemoteData as Remote
import String.Extra exposing (unquote)
import String.Interpolate exposing (interpolate)
import View.Error as ErrorView
import View.Messages exposing (viewMessages)


coloredStatusButton : String -> String -> Html a
coloredStatusButton input labelType =
    span [ class ("label label-" ++ labelType) ] [ text input ]


explainer : Config -> String -> Html msg
explainer config name =
    Markdown.toHtml [] (Maybe.withDefault "" (Dict.get name config.explainerContent))


explainerFormat : Config -> String -> List a -> Html msg
explainerFormat config name inputList =
    let
        configuredContent =
            Maybe.withDefault "" (Dict.get name config.explainerContent)

        stringList =
            List.map toString inputList |> List.map unquote
    in
    interpolate configuredContent stringList
        |> Markdown.toHtml []


makeCollapsible : String -> Bool -> Html msg -> Html msg
makeCollapsible elementId expanded view =
    ul [ id elementId, classList [ ( "collapse", True ), ( "in", expanded ) ], style [ ( "list-style-type", "none" ) ] ]
        [ li []
            [ view ]
        ]


expandedMessagesTable : String -> List Message -> Html msg
expandedMessagesTable elementId messages =
    div []
        [ h5 [ attribute "role" "button", attribute "data-toggle" "collapse", attribute "href" ("#" ++ elementId), attribute "aria-expanded" "true", attribute "aria-controls" elementId ]
            [ text "Messages"
            , i [ class "fa fa-angle-down" ] []
            ]
        , makeCollapsible elementId True <| viewMessages messages
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


errorOrView : Remote.WebData a -> (a -> Html msg) -> Html msg
errorOrView request view =
    case request of
        Remote.Success resp ->
            view resp

        Remote.Loading ->
            div [ class "loading--line" ] []

        Remote.Failure err ->
            ErrorView.viewRemoteError request

        _ ->
            div [] []
