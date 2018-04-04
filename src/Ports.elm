port module Ports exposing (HeaderValues, fileContentRead, fileSaved, highlightIds, log, objectRetrieved, prismHighlight, requestSaveFile, requestSent, responseReceived, retrieveObject, scrollIntoView, setHeaderValues, setPageTitle, storeObject, uploadFileSelected)

import Json.Decode
import Json.Encode


port log : String -> Cmd msg


port requestSent : (Json.Encode.Value -> msg) -> Sub msg


port responseReceived : (Json.Encode.Value -> msg) -> Sub msg


port prismHighlight : () -> Cmd msg


port uploadFileSelected : ( String, Int ) -> Cmd msg


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


port setPageTitle : String -> Cmd msg


port scrollIntoView : String -> Cmd msg


port highlightIds : List String -> Cmd msg


type alias HeaderValues =
    { userName : String
    , overviewLink : String
    , referLink : String
    , logoutLink : String
    }


port setHeaderValues : HeaderValues -> Cmd msg
