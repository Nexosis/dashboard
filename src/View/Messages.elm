module View.Messages exposing (viewMessages)

import Data.Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


viewMessages : List Message -> Html msg
viewMessages =
    viewMessagesInternal False


viewMessagesInternal : Bool -> List Message -> Html msg
viewMessagesInternal collapsed messages =
    let
        labelType : Message -> String
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

        messageEntry : Message -> Html msg
        messageEntry message =
            tr []
                [ td [ class "per10 left" ]
                    [ span [ class ("label label-" ++ labelType message ++ " mr5") ]
                        [ text (toString message.severity) ]
                    ]
                , td [ class "left" ]
                    [ text message.message ]
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
