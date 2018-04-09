module Util exposing ((=>), commaFormatInteger, dataSizeWithSuffix, dateToUtcDateTime, delayTask, formatDateWithTimezone, formatDisplayName, formatDisplayNameWithWidth, formatFloatToString, getTimezoneFromDate, isActuallyInteger, isJust, spinner, styledNumber, tryParseAndFormat, unwrapErrors)

import Data.DisplayDate exposing (toShortDateTimeString)
import Date exposing (Date, Month)
import Html
import Html.Attributes
import List.Extra as List
import Process
import Regex
import String.Extra exposing (ellipsis)
import Task
import Time
import Time.DateTime as DateTime exposing (DateTime, zero)
import Time.TimeZone as TimeZone exposing (TimeZone)
import Time.TimeZones as TimeZones exposing (utc)
import Time.ZonedDateTime as ZonedDateTime


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


spinner : Html.Html msg
spinner =
    Html.i [ Html.Attributes.class "fa fa-spinner fa-spin fa-2x fa-fw" ] []


dataSizeWithSuffix : Int -> String
dataSizeWithSuffix size =
    if size == 0 then
        " - "
    else
        let
            k =
                size // 1024
        in
        case k of
            0 ->
                commaFormatInteger size ++ " B"

            _ ->
                if k >= 10000 then
                    commaFormatInteger (k // 1024) ++ " Mb"
                else
                    commaFormatInteger k ++ " Kb"


commaFormatInteger : Int -> String
commaFormatInteger value =
    String.join "," (splitThousands (toString value))


splitThousands : String -> List String
splitThousands integers =
    let
        reversedSplitThousands : String -> List String
        reversedSplitThousands value =
            if String.length value > 3 then
                value
                    |> String.dropRight 3
                    |> reversedSplitThousands
                    |> (::) (String.right 3 value)
            else
                [ value ]
    in
    integers
        |> reversedSplitThousands
        |> List.reverse


formatFloatToString : Float -> String
formatFloatToString input =
    if not <| isActuallyInteger input then
        let
            expand =
                toString (ceiling (input * 100000))

            len =
                String.length expand

            filled =
                String.padLeft 5 '0' expand

            result =
                trimRightZeroes (String.left (len - 5) filled ++ "." ++ String.right 5 filled)
        in
        if String.left 1 result == "." then
            "0" ++ result
        else
            result
    else
        commaFormatInteger <| truncate input


trimRightZeroes : String -> String
trimRightZeroes input =
    let
        strings =
            String.split "." input

        left =
            Maybe.withDefault "" (List.head strings)

        right =
            Maybe.withDefault [] (List.tail strings)
    in
    if right == [ "" ] then
        left
    else
        case String.reverse input |> String.uncons of
            Just ( h, tl ) ->
                if h == '0' then
                    trimRightZeroes <| String.reverse tl
                else
                    input

            Nothing ->
                ""


isActuallyInteger : Float -> Bool
isActuallyInteger input =
    (input / 1.0 - (toFloat <| round input)) == 0


styledNumber : String -> Html.Html msg
styledNumber input =
    Html.span [ Html.Attributes.class "number" ] [ Html.text (tryParseAndFormat input) ]


unwrapErrors : Result (List err) a -> List err
unwrapErrors result =
    case result of
        Ok _ ->
            []

        Err errors ->
            errors


tryParseAndFormat : String -> String
tryParseAndFormat input =
    let
        dateCandidate =
            ZonedDateTime.fromISO8601 (utc ()) input
    in
    case dateCandidate of
        Result.Ok date ->
            toShortDateTimeString date

        _ ->
            input


delayTask : Int -> Task.Task x ()
delayTask seconds =
    Process.sleep (Time.second * toFloat seconds)


getTimezoneFromDate : Maybe String -> TimeZone
getTimezoneFromDate dateString =
    -- this is going to parse out the bits of the end date to get to a time zone that we can use to modify
    -- the dates in the data before display
    let
        offset =
            case dateString of
                Just date ->
                    date
                        |> Regex.find Regex.All (Regex.regex "(\\+|-)\\d?\\d:\\d\\d")
                        |> List.map .match
                        |> List.last
                        |> Maybe.withDefault "+00:00"

                _ ->
                    "+00:00"

        -- HACK: this is done because of strangeness in the timezone list where "Etc/GMT+offset" timezones
        -- have the opposite sign in the name to match POSIX spec
        invertString val =
            if val == "-" then
                "+"
            else
                "-"

        plusMinus =
            offset |> String.slice 0 1 |> invertString

        tzOffset =
            offset |> String.slice 1 10 |> String.split ":" |> List.head |> Maybe.withDefault "0" |> String.toInt |> Result.withDefault 0 |> toString
    in
    ("Etc/GMT" ++ plusMinus ++ tzOffset) |> TimeZones.fromName |> Maybe.withDefault (utc ())


formatDateWithTimezone : TimeZone -> Maybe String -> Maybe String
formatDateWithTimezone tzOffset input =
    -- handle if the date value isn't there, or format in right TZ
    case input of
        Just v ->
            v
                |> ZonedDateTime.fromISO8601 tzOffset
                |> Result.withDefault (ZonedDateTime.zonedDateTime (utc ()) ZonedDateTime.zero)
                |> ZonedDateTime.toISO8601
                |> Just

        Nothing ->
            Nothing


dateToUtcDateTime : TimeZone -> Date -> DateTime
dateToUtcDateTime timeZone date =
    {- This is a bit hacky -
       1. first drop the 'local' timezone from the date input to generate a DateTime at UTC
       2. then figure out the offset given the timezone,
       3. finally add the offset to the DateTime to get the right date
       This is intended to be used with ZonedDateTime to get the date at the right time once converted to a ZonedDateTime
    -}
    let
        toDate input =
            DateTime.dateTime { zero | year = Date.year input, month = monthToInt (Date.month input), day = Date.day input, hour = Date.hour input, minute = Date.minute input, second = Date.second input }

        getOffset time tz =
            TimeZone.offset (time |> DateTime.toTimestamp) tz

        dateTime =
            toDate date
    in
    DateTime.addMilliseconds (getOffset dateTime timeZone) dateTime


monthToInt : Date.Month -> Int
monthToInt value =
    case value of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


formatDisplayName : String -> String
formatDisplayName =
    formatDisplayNameWithWidth 35


formatDisplayNameWithWidth : Int -> String -> String
formatDisplayNameWithWidth len input =
    ellipsis len input
