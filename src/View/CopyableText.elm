module View.CopyableText exposing (copyableText)

import Html exposing (Html, a, i, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


copyableText : String -> (String -> msg) -> Html msg
copyableText content copyMsg =
    span [ class "small" ]
        [ text content
        , a []
            [ i [ class "fa fa-copy color-mediumgray ml5", onClick (copyMsg content) ] []
            ]
        ]
