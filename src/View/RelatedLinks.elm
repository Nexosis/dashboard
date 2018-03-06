module View.RelatedLinks exposing (..)

import AppRoutes as Routes exposing (fromApiUrl, routeToString)
import Data.Config exposing (Config)
import Data.Link exposing (Link)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData as Remote
import String.Extra as Extras exposing (replace)


type alias LinkContainer a =
    { a
        | links : List Link
    }


view : Config -> Remote.WebData (LinkContainer a) -> Html msg
view config linkedEntityResponse =
    div [ class "col-sm-3", id "related" ]
        [ h5 [ class "mt15 mb15" ] [ text "Related" ]
        , div [ class "accordion" ]
            [ div [ class "panel" ]
                [ div [ class "panel-heading" ] (layout config linkedEntityResponse)
                ]
            ]
        ]


layout : Config -> Remote.WebData (LinkContainer a) -> List (Html msg)
layout config linkedEntityResponse =
    case linkedEntityResponse of
        Remote.Success entity ->
            [ h4 [ class "related-section" ]
                [ a
                    [ href "#sessions" ]
                    [ i [ class "fa fa-plus-circle" ] [], text (listType config entity.links) ]
                ]
            , div [ class "panel-collapse collapse in" ]
                [ div [ class "panel-body" ]
                    [ ul []
                        (List.map (linkList config) entity.links)
                    ]
                ]
            ]

        Remote.Loading ->
            [ h4 [ class "related-section" ]
                [ a
                    [ href "#sessions" ]
                    []
                ]
            ]

        _ ->
            []


listType : Config -> List Link -> String
listType config links =
    case List.head links of
        Just link ->
            Extras.toTitleCase (extractLinkEntity config link.href)

        Nothing ->
            "Sessions"


linkList : Config -> Link -> Html msg
linkList config link =
    case link.rel of
        "train" ->
            listItem config link "session"

        "data" ->
            listItem config link "datasource"

        "model" ->
            listItem config link "model"

        "self" ->
            listItem config link (extractLinkEntity config link.href)

        _ ->
            li [] []


listItem : Config -> Link -> String -> Html msg
listItem config link input =
    li []
        [ a [ href (linkTransform config link) ] [ text input ]
        ]


linkTransform : Config -> Link -> String
linkTransform config link =
    let
        a =
            Debug.log "href" (Extras.replace "v1" "v2" config.baseUrl)
    in
    case fromApiUrl config.baseUrl link.href of
        Just route ->
            routeToString route

        Nothing ->
            ""


extractLinkEntity : Config -> String -> String
extractLinkEntity config input =
    case String.left 5 (Extras.replace config.baseUrl "" input) of
        "/sess" ->
            "session"

        "/data" ->
            "data"

        "/mode" ->
            "model"

        "/voca" ->
            "vocabulary"

        _ ->
            ""
