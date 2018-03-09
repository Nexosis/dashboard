module Data.Ziplist exposing (Ziplist, advance, create, find, rewind)

import List.Extra as List


type alias Ziplist a =
    { previous : List a
    , current : a
    , next : List a
    }


advance : Ziplist a -> Ziplist a
advance ziplist =
    let
        current =
            List.head ziplist.next
    in
    case current of
        Just a ->
            { previous = ziplist.previous ++ [ ziplist.current ]
            , current = a
            , next = List.tail ziplist.next |> Maybe.withDefault []
            }

        Nothing ->
            ziplist


rewind : Ziplist a -> Ziplist a
rewind ziplist =
    let
        current =
            List.last ziplist.previous
    in
    case current of
        Just a ->
            { previous = List.take (List.length ziplist.previous - 1) ziplist.previous
            , current = a
            , next = [ ziplist.current ] ++ ziplist.next
            }

        Nothing ->
            ziplist


create : List a -> a -> List a -> Ziplist a
create prev current rest =
    { previous = prev
    , current = current
    , next = rest
    }


first : Ziplist a -> Ziplist a
first ({ previous, current, next } as zipper) =
    case List.reverse previous of
        [] ->
            zipper

        y :: ys ->
            create [] y (ys ++ [ current ] ++ next)


find : (a -> Bool) -> Ziplist a -> Maybe (Ziplist a)
find pred ziplist =
    let
        resetToFirst =
            first ziplist
    in
    findItem pred resetToFirst


findItem : (a -> Bool) -> Ziplist a -> Maybe (Ziplist a)
findItem pred ziplist =
    if pred ziplist.current then
        Just ziplist
    else
        let
            nextZiplist =
                advance ziplist
        in
        if nextZiplist == ziplist then
            Nothing
        else
            findItem pred nextZiplist
