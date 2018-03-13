module Clipboard exposing (Msg(..), handle)

import Ports


type Msg
    = Copy String


handle : Msg -> Cmd msg
handle msg =
    case msg of
        Copy text ->
            Ports.copy text
