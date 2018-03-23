module View.Page exposing (ActivePage(..), basicLayout, emptyLayout, layoutShowingResponses)

import AppRoutes
import Data.Config exposing (Config)
import Data.Context as AppContext exposing (ContextModel)
import Data.Response as Response exposing (GlobalMessage, Response)
import Feature exposing (Feature)
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
    { a
        | lastRequest : String
        , lastResponse : Maybe Response
        , messages : List GlobalMessage
        , context : ContextModel
        , enabledFeatures : List Feature
    }


emptyLayout : ActivePage -> Html msg -> Html msg
emptyLayout page content =
    div [ id "docs-container", class "layout" ]
        [ viewHeader Nothing headerLinks
        , div [ class "layout-row layout-row-content" ]
            [ content
            ]
        ]


{-| Take a page's Html and layout it with a header and footer.

isLoading can be used to show loading during slow transitions

-}
layoutShowingResponses : PageValues a -> ActivePage -> Html msg -> Html msg
layoutShowingResponses pageValues page content =
    div [ id "docs-container", class "layout" ]
        [ viewHeader (Just pageValues.context.config) headerLinks
        , div [ class "layout-row layout-row-content" ]
            [ content
            ]

        --todo - put messages back in
        --, div [] [ viewMessages pageValues.messages ]
        ]


headerLinks : List (Html msg)
headerLinks =
    [ li [] [ a [ AppRoutes.href AppRoutes.DataSets ] [ text "Datasets" ] ]
    , li [] [ a [ AppRoutes.href AppRoutes.Sessions ] [ text "Sessions" ] ]
    , li [] [ a [ AppRoutes.href AppRoutes.Models ] [ text "Models" ] ]
    ]


basicLayout : ActivePage -> Html msg -> Html msg
basicLayout page content =
    div [ id "docs-container", class "layout" ]
        [ viewHeader Nothing []
        , div [ class "layout-row layout-row-content" ]
            [ div [] [ content ]
            ]
        ]


viewHeader : Maybe Config -> List (Html msg) -> Html msg
viewHeader config navLinks =
    div [ class "layout-row" ]
        [ div [ class "zone-header layout" ]
            [ header [ id "topnav" ]
                [ nav [ class "nav navbar-fixed-top", attribute "role" "navigation" ]
                    [ div [ class "container" ]
                        [ div [ class "navbar-header" ]
                            [ button
                                [ class "navbar-toggle collapsed"
                                , attribute "data-toggle" "collapse"
                                , attribute "data-target" "#topnav-collapse"
                                , attribute "aria-expanded" "false"
                                ]
                                [ span [ class "sr-only" ] [ text "Toggle navigation" ]
                                , i [ class "fa fa-bars fa-lg" ] []
                                ]
                            , a [ AppRoutes.href AppRoutes.Home, class "navbar-brand" ]
                                [ img [ src "https://nexosis.com/assets/img/logo-horizontal.png", alt "" ] []
                                , div [ class "badge badge-danger badge-logo" ] [ text "API" ]
                                ]
                            ]
                        , div [ id "topnav-collapse", class "collapse navbar-collapse" ]
                            [ ul [ class "nav navbar-nav navbar-left" ]
                                (navLinks
                                    ++ [ li []
                                            [ a
                                                [ href "#"
                                                , class "dropdown-toggle"
                                                , attribute "data-toggle" "dropdown"
                                                , attribute "role" "button"
                                                , attribute "aria-haspopup" "true"
                                                , attribute "aria-expanded" "false"
                                                ]
                                                [ text "Developers ", span [ class "caret" ] [] ]
                                            , ul [ class "dropdown-menu" ]
                                                [ li [] [ a [ href "http://docs.nexosis.com" ] [ text "Documentation" ] ]
                                                , li [] [ a [ href "http://docs.nexosis.com/guides/quickstartguide" ] [ text "Quickstart" ] ]
                                                , li [] [ a [ href "http://docs.nexosis.com/guides/" ] [ text "Guides" ] ]
                                                , li [] [ a [ href "http://docs.nexosis.com/tutorials/" ] [ text "Tutorials" ] ]
                                                , li [] [ a [ href "http://docs.nexosis.com/clients/" ] [ text "API Clients" ] ]
                                                , li [] [ a [ href "https://developers.nexosis.com/docs/services/98847a3fbbe64f73aa959d3cededb3af/" ] [ text "API Reference" ] ]
                                                , li [] [ a [ href "https://community.nexosis.com", target "_blank" ] [ text "Community" ] ]
                                                ]
                                            ]
                                       , li []
                                            [ a [ href "https://support.nexosis.com", target "_blank" ] [ text "Help" ] ]
                                       ]
                                )
                            , ul [ class "nav navbar-nav navbar-right mr10", attribute "role" "navigation", attribute "aria-label" "Account menu" ]
                                [ viewJust
                                    (\c ->
                                        li [ class "dropdown" ]
                                            [ a [ href "#", class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "aria-expanded" "false" ] [ text <| Maybe.withDefault "Account" <| Maybe.map .name c.identityToken, b [ class "caret" ] [] ]
                                            , ul [ class "dropdown-menu" ]
                                                [ li [] [ a [ href <| c.accountSiteUrl ++ "/apiaccount/accountstatus" ] [ text "Overview" ] ]
                                                , li [] [ a [ href <| c.accountSiteUrl ++ "/apiaccount/referAFriend" ] [ text "Refer a friend" ] ]
                                                , li [] [ a [ href <| c.accountSiteUrl ++ "/account/logout?returnUrl=" ++ c.apiManagerUrl ++ "/signout" ] [ text "Sign Out" ] ]
                                                ]
                                            ]
                                    )
                                    config
                                ]
                            ]
                        ]
                    ]
                ]
            ]
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
