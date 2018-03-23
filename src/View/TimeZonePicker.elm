module View.TimeZonePicker exposing (timeZoneList)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Extra as List
import String.Extra as String
import Time.TimeZones exposing (all)


tzToOption : String -> String -> Html msg
tzToOption current tzName =
    let
        -- the elm-time package seems to have the sign on the timezones backward. so the label
        -- is changed to select the 'opposite' sign TZ.
        -- discussed how this is possibly correct for the lib, but confusing to users in this issue:
        -- https://github.com/elm-community/elm-time/issues/57.
        rename input =
            if String.contains "-" input then
                String.replace "-" "+" input
            else
                String.replace "+" "-" input
    in
    option
        [ value tzName, selected (current == tzName) ]
        [ text (tzName |> String.split "/" |> List.last |> Maybe.withDefault tzName |> rename) ]


timeZoneList : (String -> msg) -> String -> Html msg
timeZoneList msg currentSelection =
    let
        opt =
            tzToOption currentSelection
    in
    select [ onInput msg ] (List.map opt (Dict.keys all |> List.filter (\k -> String.startsWith "Etc" k)))
