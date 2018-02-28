module Data.DisplayDate exposing (dateDecoder, toShortDateString)

import Json.Decode exposing (Decoder, andThen, fail, string, succeed)
import Time.TimeZones exposing (etc_universal)
import Time.ZonedDateTime exposing (ZonedDateTime, day, fromISO8601, month, toISO8601, year)


dateDecoder : Decoder ZonedDateTime
dateDecoder =
    let
        convert : String -> Decoder ZonedDateTime
        convert raw =
            case fromISO8601 (etc_universal ()) raw of
                Ok date ->
                    succeed date

                Err error ->
                    fail error
    in
    string |> andThen convert


toShortDateString : ZonedDateTime -> String
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
