module Data.Ziplist exposing (Ziplist, advance, rewind, create)

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


create : a -> List a -> Ziplist a
create first rest =
    { previous = []
    , current = first
    , next = rest
    }
