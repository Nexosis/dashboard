module View.Page exposing (ActivePage(..), basicLayout, layoutShowingResponses)

import Data.Response as Response
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Route


type ActivePage
    = Other
    | Home
    | DataSets
    | DataSetData
    | Imports
    | Sessions
    | Models


type alias PageValues a =
    { a
        | lastRequest : String
        , lastResponse : Maybe Response.Response
    }


{-| Take a page's Html and layout it with a header and footer.

isLoading can be used to show loading during slow transitions

-}
layoutShowingResponses : PageValues a -> ActivePage -> Html msg -> Html msg
layoutShowingResponses pageValues page content =
    div []
        [ viewHeader page
        , div [] [ content ]
        , viewFooter pageValues
        ]


basicLayout : ActivePage -> Html msg -> Html msg
basicLayout page content =
    div []
        [ viewHeader page
        , div [] [ content ]
        ]


viewHeader : ActivePage -> Html msg
viewHeader page =
    nav []
        [ div []
            [ a [ Route.href Route.Home ]
                [ text "Home" ]
            ]
        , hr [] []
        ]


viewFooter : PageValues a -> Html msg
viewFooter pageValues =
    let
        responseView =
            case pageValues.lastResponse of
                Just response ->
                    viewRequestResponse response

                Nothing ->
                    div [] []
    in
    footer []
        [ responseView
        ]


viewRequestResponse : Response.Response -> Html msg
viewRequestResponse response =
    div []
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ div [ class "panel" ]
                    [ div [ class "panel-body" ]
                        [ h3 [] [ text (response.method ++ " " ++ response.url) ]
                        ]
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ div [ class "panel" ]
                    [ div [ class "panel-header" ]
                        [ h4 (balloonTooltip "Raw response body") [ text "Response Body" ]
                        ]
                    , div [ class "panel-body" ]
                        [ div [] [ viewJsonSyntaxHighlightedView response ]
                        ]
                    ]
                ]
            ]
        ]


balloonTooltip : String -> List (Attribute msg)
balloonTooltip text =
    [ attribute "data-balloon" text, attribute "data-balloon-pos" "up" ]


viewJsonSyntaxHighlightedView : Response.Response -> Html msg
viewJsonSyntaxHighlightedView response =
    {-
        HACK:

       This uses a Keyed node.  In Elm we aren't supposed to modify the DOM after Elm has rendered it.
       It uses a ShadowDom to track diffs between state changes, so messing with the DOM will break that process.

       However, we want to use the Prism lib to do syntax highlighting on code, which will change the DOM within this node.

       But, if we use a Keyed note, we can flag this node with an ID.  When the ID changes, Elm will
       just throw away the old contents of this node and replace it with a new version.  It doesn't
       bother with calculating a diff of the children to see if anything needs to be redrawn.
    -}
    Html.Keyed.node
        "div"
        []
        [ ( response.timestamp
          , pre []
                [ code [ class "language-json" ] [ text response.response ]
                ]
          )
        ]
