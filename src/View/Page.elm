module View.Page exposing (ActivePage(..), layout)

import Html exposing (..)
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


{-| Take a page's Html and layout it with a header and footer.

isLoading can be used to show loading during slow transitions

-}
layout : ActivePage -> Maybe Http.Error -> String -> Html msg -> Html msg
layout page error lastRequest content =
    div []
        [ viewHeader page
        , div [] [ content ]
        , viewFooter error lastRequest
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


viewFooter : Maybe Http.Error -> String -> Html msg
viewFooter error lastRequest =
    footer []
        [ div [] [ text lastRequest ]
        ]
