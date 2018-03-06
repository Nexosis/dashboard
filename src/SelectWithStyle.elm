--Taken from https://raw.githubusercontent.com/lgastako/elm-select/ but updated to allow a CSS style on select


module SelectWithStyle exposing (from, fromSelected, fromSelected_, from_)

{-| This module provides the `from` and `fromSelected` (and more
flexible `from_` and `fromSelected_`) helpers to make working
with `select` elements in Elm easier.

[See full examples in github](https://github.com/lgastako/elm-select/blob/master/src/Main.elm)

@docs from, from_, fromSelected, fromSelected_

-}

import Html exposing (Html, option, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)


{-| Convert a list of values and stringifying function for values of that type
into a `select` element which contains as its values the list of values.

    from [North, South, East, West] Direction

would produce a select element with box option values and labels being the
string versions of four cardinal directions and which would send messages like
`Direction North` or `Direction East` when the user selects new directions from
the drop down.

-}
from : List a -> (a -> msg) -> Html msg
from xs msg =
    from_ xs msg toString toString


{-| Convert a list of values and stringifying function for values of that type
into a `select` element which contains as its values the list of values, with
a specified element currently selected.

    from [North, South, East, West] Direction South

would produce a select element with box option values and labels being the
string versions of four cardinal directions and which would send messages like
`Direction North` or `Direction East` when the user selects new directions from
the drop down. `South` would show up as the currently selected element.

-}
fromSelected : String -> List a -> (a -> msg) -> a -> Html msg
fromSelected css xs msg sel =
    fromSelected_ css xs msg toString toString sel


{-| Convert a list of values and stringifying function for values of that type
into a `select` element which contains as its values the list of values.

    from_ [North, South, East, West] Direction toId toLabel

like the `from` example above, this would produce a select element with box
option values and labels being derived from the four cardinal directions and
which would send messages like `Direction North` or `Direction East` when the
user selects new directions from the drop down only the ids and labels would be
derived using the `toId` and `toLabel` functions provided instead of defaulting
to `toString` like `from`.

-}
from_ : List a -> (a -> msg) -> (a -> String) -> (a -> String) -> Html msg
from_ xs msg toId toLabel =
    let
        optionize x =
            option [ (value << toId) x ]
                [ (text << toLabel) x ]
    in
    select [ onInput (msg << makeFromString_ xs) ]
        (List.map optionize xs)


{-| Convert a list of values and stringifying function for values of that type
into a `select` element which contains as its values the list of values, with
a specified element currently selected.

    fromSelected_ [North, South, East, West] Direction toId toLabel South

like the `from` example above, this would produce a select element with box
option values and labels being derived from the four cardinal directions and
which would send messages like `Direction North` or `Direction East` when the
user selects new directions from the drop down only the ids and labels would be
derived using the `toId` and `toLabel` functions provided instead of defaulting
to `toString` like `from`. `South` would show up as the currently selected element.

-}
fromSelected_ : String -> List a -> (a -> msg) -> (a -> String) -> (a -> String) -> a -> Html msg
fromSelected_ css xs msg toId toLabel sel =
    let
        optionize x =
            option
                ([ (value << toId) x ]
                    ++ (if x == sel then
                            [ selected True ]
                        else
                            []
                       )
                )
                [ (text << toLabel) x ]
    in
    select ([ onInput (msg << makeFromString_ xs) ] ++ [ class css ])
        (List.map optionize xs)


makeFromString : List a -> (String -> Maybe a)
makeFromString xs =
    let
        fromString s =
            let
                matches =
                    List.filter (\x -> toString x == s) xs
            in
            List.head matches
                |> Maybe.map
                    (\match ->
                        if List.length matches > 1 then
                            Debug.crash "> 1 match in generic fromString"
                        else
                            match
                    )
    in
    fromString


makeFromString_ : List a -> (String -> a)
makeFromString_ xs =
    let
        fromString =
            makeFromString xs

        fromString_ s =
            case fromString s of
                Nothing ->
                    Debug.crash "fromString returned Nothing in fromString_"

                Just s ->
                    s
    in
    fromString_
