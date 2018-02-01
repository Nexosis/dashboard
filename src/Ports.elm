port module Ports exposing (prismHighlight, requestSent, responseReceived)

import Json.Encode


port requestSent : (Json.Encode.Value -> msg) -> Sub msg


port responseReceived : (Json.Encode.Value -> msg) -> Sub msg


port prismHighlight : () -> Cmd msg
