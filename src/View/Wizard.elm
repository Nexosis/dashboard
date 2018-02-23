module View.Wizard exposing (WizardConfig, viewButtons)

import Data.Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import View.Extra exposing (viewIf)


type alias WizardConfig msg =
    { nextMessage : msg
    , prevMessage : msg
    }


viewButtons : WizardConfig msg -> Bool -> Ziplist a -> Html msg
viewButtons wizardConfig canAdvance ziplist =
    let
        allowNext =
            List.length ziplist.next > 0 && canAdvance

        allowPrev =
            List.length ziplist.previous > 0

        nextVisible =
            List.length ziplist.next > 0
    in
    div [ class "form-group" ]
        [ button
            [ class "btn secondary"
            , onClick wizardConfig.prevMessage
            , tabindex -1
            , disabled <| not allowPrev
            ]
            [ i [ class "fa fa-chevron-left mr5" ] [], text "Previous" ]
        , viewIf
            (\() ->
                button
                    [ class "btn"
                    , onClick wizardConfig.nextMessage
                    , disabled <| not allowNext
                    ]
                    [ text "Next", i [ class "fa fa-chevron-right ml5" ] [] ]
            )
            nextVisible
        ]
