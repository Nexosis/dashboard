module View.Page exposing (ActivePage(..), layout)

import AppRoutes
import Data.Context as AppContext exposing (ContextModel)
import Data.Response as Response exposing (GlobalMessage, Response)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import View.Extra exposing (viewIfElements, viewJust)


type ActivePage
    = Other
    | Home
    | DataSets
    | DataSetData
    | DataSetAdd
    | Imports
    | Sessions
    | SessionDetail
    | SessionStart
    | Models
    | ModelDetail


type alias PageValues a =
    { a | context : ContextModel }


{-| Take a page's Html and layout it with a header and footer.

isLoading can be used to show loading during slow transitions

-}
layout : ActivePage -> Html msg -> Html msg
layout page content =
    div [ id "docs-container", class "layout" ]
        [ div [ class "layout-row layout-row-content" ]
            [ content ]

        --todo - put messages back in
        --, div [] [ viewMessages pageValues.messages ]
        ]


viewMessages : List GlobalMessage -> Html msg
viewMessages messages =
    div []
        [ viewIfElements
            (\() ->
                ul [] (messages |> List.map viewMessage)
            )
            messages
        ]


viewMessage : GlobalMessage -> Html msg
viewMessage message =
    li []
        [ div []
            --todo - will probably want to translate these into icons of some kind.
            [ text (toString message.severity)
            ]
        , viewJust (\route -> a [ AppRoutes.href route ] [ text message.message ]) message.routeToResource
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
                        [ h4 [] [ text "Response Body" ]
                        ]
                    , div [ class "panel-body" ]
                        [ div [] [ viewJsonSyntaxHighlightedView response ]
                        ]
                    ]
                ]
            ]
        ]


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
