port module Interop exposing (..)

import Json.Encode as J


port storeObject : ( String, J.Value ) -> Cmd msg


port retrieveObject : String -> Cmd msg


port objectRetrieved : (( String, J.Value ) -> msg) -> Sub msg
