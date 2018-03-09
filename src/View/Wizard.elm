module View.Wizard exposing (WizardConfig, WizardProgressConfig, viewButtons, viewProgress)

import Data.Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra as ListX
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
            [ class "btn btn-primary"
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


type alias WizardProgressConfig a =
    { stepDescriptions : List ( a, String )
    }


viewProgress : WizardProgressConfig a -> Ziplist a -> Html Never
viewProgress config zipList =
    let
        completedItems =
            zipList.previous
                |> List.map (convertToDesc config.stepDescriptions)
                |> List.map (viewItem Done)

        currentItem =
            zipList.current
                |> convertToDesc config.stepDescriptions
                |> viewItem InProgress

        nextItems =
            zipList.next
                |> List.map (convertToDesc config.stepDescriptions)
                |> List.map (viewItem Pending)
    in
    div [ class "progress-indicator" ]
        [ ul [ class "list-inline" ]
            (List.concat
                [ completedItems, [ currentItem ], nextItems ]
            )
        ]


convertToDesc : List ( a, String ) -> a -> String
convertToDesc descriptions step =
    descriptions
        |> ListX.find (\( a, desc ) -> a == step)
        |> Maybe.map Tuple.second
        |> Maybe.withDefault (toString step)


viewItem : ProgressState -> String -> Html Never
viewItem progressState description =
    let
        ( color, icon ) =
            case progressState of
                Done ->
                    ( "text-success", "fa fa-check-circle" )

                InProgress ->
                    ( "text-danger", "fa fa-circle-o" )

                Pending ->
                    ( "", "fa fa-circle-o" )
    in
    li [ class color ]
        [ i [ class icon ] []
        , text description
        ]


type ProgressState
    = Done
    | InProgress
    | Pending
