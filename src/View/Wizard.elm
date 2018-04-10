module View.Wizard exposing (HtmlDetails, StepValidator, WizardConfig, WizardProgressConfig, viewButtons, viewProgress)

import Data.Context as AppContext exposing (ContextModel)
import Data.Ziplist exposing (Ziplist)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra as ListX
import Util exposing (spinner)
import View.Extra exposing (viewIf)


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type alias WizardConfig a error msg model result =
    { nextMessage : msg
    , prevMessage : msg
    , stepValidation : List ( a, StepValidator model error )
    , finishedValidation : model -> Result (List error) result
    , finishedButton : model -> HtmlDetails msg
    , finishedMsg : result -> msg
    , customLoading : Maybe (model -> Html msg)
    }


type alias StepValidator model error =
    model -> List error


viewButtons : WizardConfig a b msg model request -> model -> Ziplist a -> Bool -> Bool -> Html msg
viewButtons wizardConfig model ziplist showLoading currentStepValid =
    let
        nextAttributes =
            if currentStepValid then
                [ class "btn btn-danger", onClick <| wizardConfig.nextMessage ]
            else
                [ class "btn" ]

        allowPrev =
            List.length ziplist.previous > 0

        nextVisible =
            List.length ziplist.next > 0

        finishedAttributes =
            case wizardConfig.finishedValidation model of
                Ok validModel ->
                    [ class "btn btn-danger", onClick <| wizardConfig.finishedMsg validModel ]

                Err errors ->
                    [ class "btn" ]

        finishedButton =
            if showLoading then
                button [ class "btn" ] [ model |> Maybe.withDefault (\a -> spinner) wizardConfig.customLoading ]
            else
                let
                    finishedButtonContents =
                        wizardConfig.finishedButton model
                in
                button (finishedAttributes ++ finishedButtonContents.attributes)
                    finishedButtonContents.children
    in
    div []
        [ viewIf
            (\() ->
                button
                    [ class "btn btn-primary"
                    , onClick wizardConfig.prevMessage
                    , tabindex -1
                    ]
                    [ i [ class "fa fa-chevron-left mr5" ] [], text "Previous" ]
            )
            allowPrev
        , viewIf
            (\() ->
                button nextAttributes [ text "Next", i [ class "fa fa-chevron-right ml5" ] [] ]
            )
            nextVisible
        , viewIf (\() -> finishedButton) (not nextVisible)
        ]


type alias WizardProgressConfig a =
    { stepDescriptions : List ( a, String ) }


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
