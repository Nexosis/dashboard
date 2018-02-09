port module Ports exposing (log, prismHighlight, requestSent, responseReceived)

import Json.Encode


port log : String -> Cmd msg


port requestSent : (Json.Encode.Value -> msg) -> Sub msg


port responseReceived : (Json.Encode.Value -> msg) -> Sub msg


port prismHighlight : () -> Cmd msg
