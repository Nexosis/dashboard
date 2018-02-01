port module Ports exposing (requestSent, responseReceived)

import Json.Encode


port requestSent : (Json.Encode.Value -> msg) -> Sub msg


port responseReceived : (Json.Encode.Value -> msg) -> Sub msg
