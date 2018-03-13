module Clipboard exposing (Msg(..), copyToClipboard, handle)

import Ports


type Msg
    = Copy String


handle : Msg -> Cmd msg
handle msg =
    case msg of
        Copy text ->
            copyToClipboard text


copyToClipboard : String -> Cmd msg
copyToClipboard text =
    Ports.copy text
