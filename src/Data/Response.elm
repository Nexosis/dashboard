module Data.Response exposing (Response, decodeXhrResponse)

import Json.Decode
import Json.Decode.Pipeline


type alias Response =
    { status : Int
    , statusText : String
    , response : String
    , method : String
    , url : String
    }


decodeXhrResponse : Json.Decode.Value -> Result String Response
decodeXhrResponse value =
    Json.Decode.decodeValue decodeResponse value


decodeResponse : Json.Decode.Decoder Response
decodeResponse =
    Json.Decode.Pipeline.decode Response
        |> Json.Decode.Pipeline.required "status" Json.Decode.int
        |> Json.Decode.Pipeline.required "statusText" Json.Decode.string
        |> Json.Decode.Pipeline.required "response" Json.Decode.string
        |> Json.Decode.Pipeline.required "method" Json.Decode.string
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
