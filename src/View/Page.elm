module View.Page exposing (ActivePage(..), layout)

import Html exposing (..)
import Route exposing (Route)


type ActivePage
    = Other
    | Home
    | DataSets
    | Imports
    | Sessions
    | Models


{-| Take a page's Html and layout it with a header and footer.

isLoading can be used to slow loading during slow transitions

-}
layout : ActivePage -> Html msg -> Html msg
layout page content =
    div []
        [ viewHeader page
        , div [] [ content ]
        , viewFooter
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


viewFooter : Html msg
viewFooter =
    footer []
        [ div [] []
        ]
