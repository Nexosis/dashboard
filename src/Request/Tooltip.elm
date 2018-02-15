module Request.Tooltip exposing (getTooltipDictionary)

import Dict exposing (Dict)
import Http
import HttpBuilder exposing (get, toRequest, withExpect)
import Json.Decode exposing (Decoder, dict, string)


getTooltipDictionary : Http.Request (Dict String String)
getTooltipDictionary =
    let
        expect =
            tooltipDictDecoder
                |> Http.expectJson
    in
    "/tooltips.json"
        |> get
        |> withExpect expect
        |> toRequest


tooltipDictDecoder : Decoder (Dict String String)
tooltipDictDecoder =
    dict string
