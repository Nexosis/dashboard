module View.Tooltip exposing (helpIcon)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)


balloonTooltip : String -> List (Attribute msg)
balloonTooltip text =
    [ attribute "data-balloon" text, attribute "data-balloon-pos" "up" ]


helpIcon : Dict.Dict String String -> String -> List (Html msg)
helpIcon tooltipDefinitions tipKey =
    let
        tipText =
            Dict.get tipKey tooltipDefinitions
    in
    case tipText of
        Just text ->
            --Having a wrapper with position set to absolute helps the tooltip display without getting clipped by a parent that has overflow set to something other that 'visible'.
            [ span [ style [ ( "position", "absolute" ) ] ]
                [ span (balloonTooltip text) [ i [ class "fa fa-question-circle small color-mediumGray ml5" ] [] ]
                ]
            ]

        Nothing ->
            []
