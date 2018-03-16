module View.CopyableText exposing (copyableText)

import Html exposing (Html, a, i, span, text)
import Html.Attributes exposing (attribute, class, href)


copyableText : String -> Html msg
copyableText content =
    span [ class "small" ]
        [ text content
        , a
            [ class "btn btn-sm p0 m0 copyToClipboard"
            , attribute "role" "button"
            , attribute "data-clipboard-text" content
            ]
            [ i [ class "fa fa-copy color-mediumgray ml5" ] []
            ]
        ]
