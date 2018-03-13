module View.CopyableText exposing (copyableText)

import Clipboard
import Html exposing (Html, a, i, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


copyableText : String -> (Clipboard.Msg -> msg) -> Html msg
copyableText content msgMap =
    span [ class "small" ]
        [ text content
        , a []
            [ i [ class "fa fa-copy color-mediumgray ml5", onClick (msgMap <| Clipboard.Copy content) ] []
            ]
        ]
