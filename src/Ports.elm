port module Ports exposing (drawVegaChart, fileContentRead, log, prismHighlight, requestSent, responseReceived, uploadFileSelected)

import Json.Decode
import Json.Encode
import VegaLite exposing (Spec)


port log : String -> Cmd msg


port requestSent : (Json.Encode.Value -> msg) -> Sub msg


port responseReceived : (Json.Encode.Value -> msg) -> Sub msg


port prismHighlight : () -> Cmd msg


port drawVegaChart : Spec -> Cmd msg


port uploadFileSelected : String -> Cmd msg


port fileContentRead : (Json.Decode.Value -> msg) -> Sub msg
