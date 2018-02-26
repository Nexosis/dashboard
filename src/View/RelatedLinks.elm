module View.RelatedLinks exposing (..)

import Data.Link exposing (Link)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData as Remote


type alias LinkContainer a =
    { a
        | links : List Link
    }


view : Remote.WebData (LinkContainer a) -> Html msg
view linkedEntityResponse =
    case linkedEntityResponse of
        Remote.Success entity ->
            div [ class "col-sm-3", id "related" ]
                [ h5 [ class "mt15 mb15" ] [ text "Related" ]
                , div [ class "accordion" ]
                    [ div [ class "panel" ]
                        [ div [ class "panel-heading" ]
                            [ h4 [ class "related-section" ]
                                [ a [ href "#imports" ] [ i [ class "fa fa-plus-circle" ] [], text "Sessions" ]
                                ]
                            ]
                        , div [ class "panel-collapse collapse in" ]
                            [ div [ class "panel-body" ]
                                [ ul []
                                    (List.map linkList entity.links)
                                ]
                            ]
                        ]
                    ]
                ]

        _ ->
            span [] []


linkList : Link -> Html msg
linkList link =
    case link.rel of
        "train" ->
            li [] [ a [ href link.href ] [ text "session" ] ]

        _ ->
            li [] []
