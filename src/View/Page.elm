module View.Page exposing (ActivePage(..), layout)

import Data.Response as Response
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Http
import Route exposing (Route)


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
layout : ActivePage -> PageValues a -> Html msg -> Html msg
layout page pageValues content =
    div []
        [ viewHeader page
        , div [] [ content ]
        , viewFooter pageValues
        ]


viewHeader : ActivePage -> Html msg
viewHeader page =
    nav []
        [ div []
            [ a [ Route.href Route.Home ]
                [ text "Home" ]
            , text " | "
            , a [ Route.href Route.DataSets ]
                [ text "DataSets" ]
            , text " | "
            , a [ Route.href Route.Imports ]
                [ text "Imports" ]
            , text " | "
            , a [ Route.href Route.Sessions ]
                [ text "Sessions" ]
            , text " | "
            , a [ Route.href Route.Models ]
                [ text "Models" ]
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
                        [ h4 [] [ text "Response Body" ]
                        ]
                    , div [ class "panel-body" ]
                        [ div [] [ viewJsonSyntaxHighlightedView response.response ]
                        ]
                    ]
                ]
            ]
        ]


viewJsonSyntaxHighlightedView : String -> Html msg
viewJsonSyntaxHighlightedView content =
    Html.Keyed.node
        "div"
        []
        [ ( "responseBody"
          , pre []
                [ code [ class "language-json" ] [ text content ]
                ]
          )
        ]
