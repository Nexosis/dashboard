module View.Page exposing (ActivePage(..), layout)

import Data.Response as Response
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
        responseText =
            case pageValues.lastResponse of
                Just response ->
                    toString response

                Nothing ->
                    ""
    in
    footer []
        [ div [] [ text responseText ]
        ]
