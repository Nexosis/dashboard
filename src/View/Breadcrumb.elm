module View.Breadcrumb exposing (detail, list)

import AppRoutes
import Html exposing (Html, a, div, i, p, span, text)
import Html.Attributes exposing (class, href, style)


breadcrumb : List (Html msg) -> Html msg
breadcrumb child =
    div [ class "col-sm-12" ]
        [ p [ class "breadcrumb" ]
            [ span []
                ([ a [ AppRoutes.href AppRoutes.Home ] [ text "API Dashboard" ] ]
                    ++ child
                )
            ]
        ]


child : AppRoutes.Route -> String -> List (Html msg)
child route name =
    [ i [ class "fa fa-angle-right", style [ ( "margin", "0 5px" ) ] ] []
    , a [ AppRoutes.href route ] [ text name ]
    ]


list : Html msg
list =
    breadcrumb []


detail : AppRoutes.Route -> String -> Html msg
detail route name =
    breadcrumb <| child route name
