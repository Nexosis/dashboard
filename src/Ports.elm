port module Ports exposing (drawVegaChart, log, prismHighlight, requestSent, responseReceived)

import Json.Encode
import VegaLite exposing (Spec)


port log : String -> Cmd msg


port requestSent : (Json.Encode.Value -> msg) -> Sub msg


port responseReceived : (Json.Encode.Value -> msg) -> Sub msg


port prismHighlight : () -> Cmd msg


port drawVegaChart : Spec -> Cmd msg
