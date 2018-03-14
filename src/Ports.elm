port module Ports exposing (drawVegaChart, fileContentRead, fileSaved, log, objectRetrieved, prismHighlight, requestSaveFile, requestSent, responseReceived, retrieveObject, storeObject, uploadFileSelected)

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


type alias Filespec =
    { contents : String
    , name : String
    , contentType : String
    }


port requestSaveFile : Filespec -> Cmd msg


port fileSaved : (Bool -> msg) -> Sub msg


port storeObject : ( String, Json.Encode.Value ) -> Cmd msg


port retrieveObject : String -> Cmd msg


port objectRetrieved : (( String, Json.Encode.Value ) -> msg) -> Sub msg
