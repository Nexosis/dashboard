module Common exposing (Msg(..), logRequest)

import Http
import Task


type Msg
    = Request String


logRequest : Http.Request a -> Cmd Msg
logRequest req =
    Task.perform Request (Task.succeed (toString req))
