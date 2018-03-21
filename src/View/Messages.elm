module View.Messages exposing (messageSeverityDisplay, viewMessages)

import Data.Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias DisplayMessage a =
    { a
        | severity : Severity
        , message : String
    }


viewMessages : List (DisplayMessage a) -> Html msg
viewMessages =
    viewMessagesInternal False


viewMessagesInternal : Bool -> List (DisplayMessage a) -> Html msg
viewMessagesInternal collapsed messages =
    let
        messageEntry : DisplayMessage a -> Html msg
        messageEntry message =
            tr []
                [ td [ class "per10 left" ] [ messageSeverityDisplay message ]
                , td [ class "left" ] [ text message.message ]
                ]

        collapsedAttribute collapsed =
            if collapsed == True then
                " collapse"
            else
                ""
    in
    table [ class ("table table-striped" ++ collapsedAttribute collapsed) ]
        [ thead []
            [ tr []
                [ th [ class "per10" ]
                    [ text "Date" ]
                , th [ class "per15" ]
                    [ text "Status" ]
                ]
            ]
        , tbody []
            (List.map messageEntry messages)
        ]


messageSeverityDisplay : DisplayMessage a -> Html msg
messageSeverityDisplay message =
    let
        labelType : DisplayMessage a -> String
        labelType message =
            case message.severity of
                Debug ->
                    "info"

                Informational ->
                    "info"

                Warning ->
                    "warning"

                Error ->
                    "danger"
    in
    span [ class ("label label-" ++ labelType message ++ " mr5") ]
        [ text (toString message.severity) ]
