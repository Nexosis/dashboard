module View.Pager exposing (PagedListing, filterToPage, mapToPagedListing, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import RemoteData as Remote exposing (WebData)


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
                Remote.Success successResponse ->
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

                        first =
                            changePageMsg 0

                        last =
                            changePageMsg (successResponse.totalPages - 1)
                    in
                    div [ class "pagination" ]
                        [ div [ class "btn-group", attribute "role" "group" ]
                            (firstButton prevEnabled first
                                :: [ backButton prevEnabled prev ]
                                ++ pageButtons
                                ++ [ nextButton nextEnabled next ]
                                ++ [ lastButton nextEnabled last ]
                            )
                        ]

                _ ->
                    div [] []
    in
    pager


generatePageButtons : Int -> Int -> (Int -> msg) -> List (Html msg)
generatePageButtons currentPage numberOfPages changePageMsg =
    let
        currentPageDisplay =
            currentPage + 1

        -- Display 1 pages on either side of the current page.  This way we are always showing 3 pages that could be jumped to.
        -- Unless, we are at the beginning or the end of the page list.  Then, this is number adjusted so we still end up with 3 pages total.
        numPagesAroundCurrent =
            if currentPageDisplay < 3 then
                3 - currentPageDisplay
            else if currentPageDisplay > numberOfPages then
                2 - (numberOfPages - currentPageDisplay)
            else if currentPageDisplay == numberOfPages then
                2
            else
                1

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


firstButton : Bool -> msg -> Html msg
firstButton isEnabled msg =
    button [ class "btn btn-default", disabled (not isEnabled), onClick msg ]
        [ i [ class "fa fa-angle-left" ] [], i [ class "fa fa-angle-left" ] [] ]


lastButton : Bool -> msg -> Html msg
lastButton isEnabled msg =
    button [ class "btn btn-default", disabled (not isEnabled), onClick msg ]
        [ i [ class "fa fa-angle-right" ] [], i [ class "fa fa-angle-right" ] [] ]


type alias PagedListing a =
    { pageNumber : Int
    , totalPages : Int
    , pageSize : Int
    , totalCount : Int
    , items : List a
    }


mapToPagedListing : Int -> List a -> PagedListing a
mapToPagedListing currentPage rows =
    let
        count =
            List.length rows

        pageSize =
            10
    in
    { pageNumber = currentPage
    , totalPages = (count + pageSize - 1) // pageSize
    , pageSize = pageSize
    , totalCount = count
    , items = rows
    }


filterToPage : Remote.WebData (PagedListing a) -> List a
filterToPage model =
    case model of
        Remote.Success result ->
            let
                drop =
                    result.pageSize * result.pageNumber
            in
            result.items
                |> List.drop drop
                |> List.take result.pageSize

        _ ->
            []
