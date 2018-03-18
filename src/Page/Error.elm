module Page.Error exposing (PageLoadError, pageLoadError, view, viewError)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.
-}

import Html exposing (Html, div, h1, img, main_, p, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)
import View.Page as Page exposing (ActivePage)


-- MODEL --


type PageLoadError
    = PageLoadError Model


type alias Model =
    { activePage : ActivePage
    , errorMessage : String
    }


pageLoadError : ActivePage -> String -> PageLoadError
pageLoadError activePage errorMessage =
    PageLoadError { activePage = activePage, errorMessage = errorMessage }



-- VIEW --


view : PageLoadError -> Html msg
view (PageLoadError model) =
    viewError <| p [] [ text model.errorMessage ]


viewError : Html msg -> Html msg
viewError error =
    main_ [ id "content", class "container error-container", tabindex -1 ]
        [ div [ class "error-panel panel panel-default plain animated bounceIn" ]
            [ div [ class "panel-heading" ] [ div [ class "panel-title text-center" ] [ h1 [] [ text "Error Loading Page" ] ] ]
            , div [ class "panel-body" ]
                [ div [ class "center" ]
                    [ img [ src "https://cdn2.hubspot.net/hubfs/2307117/assets/404.png" ] []
                    ]
                , div [ class "alert alert-danger" ]
                    [ error
                    ]
                ]
            ]
        ]
