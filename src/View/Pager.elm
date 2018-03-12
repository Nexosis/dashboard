module View.Pager exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)


type alias PagedValues a =
    { a
        | pageNumber : Int
        , totalPages : Int
        , pageSize : Int
        , totalCount : Int
    }


view : WebData (PagedValues a) -> (Int -> msg) -> Html msg
view pagedValues changePageMsg =
    let
        pager =
            case pagedValues of
                RemoteData.Success successResponse ->
                    let
                        pageButtons =
                            generatePageButtons successResponse.pageNumber successResponse.totalPages changePageMsg

                        prevEnabled =
                            successResponse.pageNumber > 0

                        nextEnabled =
                            successResponse.pageNumber < (successResponse.totalPages - 1)

                        prev =
                            changePageMsg (successResponse.pageNumber - 1)

                        next =
                            changePageMsg (successResponse.pageNumber + 1)
                    in
                    div [ class "btn-group", attribute "role" "group" ]
                        (backButton prevEnabled prev
                            :: pageButtons
                            ++ [ nextButton nextEnabled next ]
                        )

                _ ->
                    let
                        changeMsg =
                            changePageMsg 0
                    in
                    div [ class "pagination" ]
                        [ div [ class "btn-group", attribute "role" "group" ]
                            [ backButton False changeMsg
                            , nextButton False changeMsg
                            ]
                        ]
    in
    pager


generatePageButtons : Int -> Int -> (Int -> msg) -> List (Html msg)
generatePageButtons currentPage numberOfPages changePageMsg =
    let
        currentPageDisplay =
            currentPage + 1

        -- Display 2 pages on either side of the current page.  This way we are always showing 5 pages that could be jumped to.
        -- Unless, we are at the beginning or the end of the page list.  Then, this is number adjusted so we still end up with 5 pages total.
        numPagesAroundCurrent =
            if currentPageDisplay < 3 then
                5 - currentPageDisplay
            else if currentPageDisplay > (numberOfPages - 2) then
                4 - (numberOfPages - currentPageDisplay)
            else
                2

        lowPageNum =
            Basics.max 1 (currentPageDisplay - numPagesAroundCurrent)

        highPageNum =
            Basics.min numberOfPages (currentPageDisplay + numPagesAroundCurrent)

        pagesToShow =
            List.range lowPageNum highPageNum
    in
    pagesToShow
        |> List.map
            (\num ->
                button
                    [ class "btn btn-default"
                    , disabled (num == currentPageDisplay)
                    , onClick (changePageMsg (num - 1))
                    ]
                    [ text (toString num) ]
            )


backButton : Bool -> msg -> Html msg
backButton isEnabled msg =
    button [ class "btn btn-default", disabled (not isEnabled), onClick msg ]
        [ i [ class "fa fa-angle-left" ] [] ]


nextButton : Bool -> msg -> Html msg
nextButton isEnabled msg =
    button [ class "btn btn-default", disabled (not isEnabled), onClick msg ]
        [ i [ class "fa fa-angle-right" ] [] ]
