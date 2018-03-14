module View.CopyableText exposing (copyableText)

import Html exposing (Html, a, i, span, text)
import Html.Attributes exposing (attribute, class)


copyableText : String -> Html msg
copyableText content =
    span [ class "small" ]
        [ text content
        , a []
            [ i [ class "fa fa-copy color-mediumgray ml5 copyToClipboard", attribute "data-clipboard-text" content ] []
            ]
        ]
