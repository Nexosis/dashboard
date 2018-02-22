module Util exposing ((=>), isJust, toShortDateString)

import Time.DateTime exposing(DateTime, year, month, day)
-- toTuple


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>


isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        Just _ ->
            True

toShortDateString : DateTime -> String
toShortDateString time =
        padded (month time)
        ++ "/"
        ++ padded (day time)
        ++ "/"
        ++ toString (year time)

padded : Int -> String
padded n =
    if n < 10 then
        "0" ++ toString n
    else
        toString n