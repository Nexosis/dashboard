module Data.Ziplist exposing (Ziplist, advance, create, rewind)

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
