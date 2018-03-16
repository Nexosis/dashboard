module View.TimeZonePicker exposing (timeZoneList)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Extra as List
import Time.TimeZones exposing (all)


tzToOption : String -> String -> Html msg
tzToOption current tzName =
    option [ value tzName, selected (current == tzName) ] [ text (tzName |> String.split "/" |> List.last |> Maybe.withDefault tzName) ]


timeZoneList : (String -> msg) -> String -> Html msg
timeZoneList msg currentSelection =
    let
        opt =
            tzToOption currentSelection
    in
    select [ onInput msg ] (List.map opt (Dict.keys all |> List.filter (\k -> String.startsWith "Etc" k)))
