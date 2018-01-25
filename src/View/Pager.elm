module View.Pager exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias PagedValues a =
    { a |
     pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
}

view : PagedValues a -> (Int -> msg) -> Html msg
view pagedValues changePageMsg =
    let
        next = pagedValues.pageNumber + 1
        prev = pagedValues.pageNumber - 1
    in
        div []
        [ button [ onClick (changePageMsg prev), disabled (pagedValues.pageNumber == 0) ] [ text "Prev" ]
        , button [ onClick (changePageMsg next), disabled (pagedValues.pageNumber >= (pagedValues.totalPages - 1)) ] [ text "Next" ]
        ]

