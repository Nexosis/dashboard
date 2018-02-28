module Data.DisplayDate exposing (dateDecoder, toShortDateString, toShortDateStringOrEmpty)

import Json.Decode exposing (Decoder, andThen, fail, string, succeed)
import Time.TimeZones exposing (etc_universal)
import Time.ZonedDateTime exposing (ZonedDateTime, day, fromDateTime, fromISO8601, month, toISO8601, year)


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


toShortDateStringOrEmpty : Maybe ZonedDateTime -> String
toShortDateStringOrEmpty maybeTime =
    case maybeTime of
        Just time ->
            toShortDateString time

        Nothing ->
            ""


padded : Int -> String
padded n =
    if n < 10 then
        "0" ++ toString n
    else
        toString n
