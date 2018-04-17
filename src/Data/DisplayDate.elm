module Data.DisplayDate exposing (toShortDateString, toShortDateStringOrEmpty, toShortDateTimeString)

import Time.ZonedDateTime exposing (ZonedDateTime, day, fromDateTime, fromISO8601, hour, minute, month, toISO8601, utcOffsetString, year)


toShortDateString : ZonedDateTime -> String
toShortDateString time =
    padded (month time)
        ++ "/"
        ++ padded (day time)
        ++ "/"
        ++ toString (year time)


toShortDateStringOrEmpty : Maybe ZonedDateTime -> String
toShortDateStringOrEmpty maybeTime =
    case maybeTime of
        Just time ->
            toShortDateString time

        Nothing ->
            ""


toShortDateTimeString : ZonedDateTime -> String
toShortDateTimeString time =
    padded (month time)
        ++ "/"
        ++ padded (day time)
        ++ "/"
        ++ toString (year time)
        ++ " "
        ++ padded (hour time)
        ++ ":"
        ++ padded (minute time)
        ++ " "
        ++ utcOffsetString time


padded : Int -> String
padded n =
    if n < 10 then
        "0" ++ toString n
    else
        toString n
