module Data.Link exposing (..)

import Json.Decode as Decode


type alias Link =
    { rel : String
    , href : String
    }


linkDecoder : Decode.Decoder Link
linkDecoder =
    Decode.map2 Link
        (Decode.field "rel" Decode.string)
        (Decode.field "href" Decode.string)
