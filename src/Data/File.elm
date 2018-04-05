module Data.File exposing (FileReadStatus(..), FileUploadErrorType(..), fileReadStatusDecoder)

import Data.Columns as Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Dict exposing (Dict)
import Json.Decode exposing (dict, keyValuePairs, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type FileUploadErrorType
    = FileTooLarge
    | UnsupportedFileType
    | UnknownError


type FileReadStatus
    = ReadError FileUploadErrorType
    | Success String String


type alias JsonFile =
    { columns : List ColumnMetadata
    , data : List (Dict String String)
    }


jsonFileDecoder : Json.Decode.Decoder JsonFile
jsonFileDecoder =
    let
        row =
            dict string
    in
    decode JsonFile
        |> required "columns" decodeColumnMetadata
        |> required "data" (list row)


fileReadStatusDecoder : Json.Decode.Decoder FileReadStatus
fileReadStatusDecoder =
    Json.Decode.field "status" Json.Decode.string
        |> Json.Decode.andThen
            (\status ->
                case status of
                    "Success" ->
                        Json.Decode.map2
                            (\f c -> Success f c)
                            (Json.Decode.field "filename" Json.Decode.string)
                            (Json.Decode.field "contents" Json.Decode.string)

                    "FileTooLarge" ->
                        Json.Decode.succeed (ReadError FileTooLarge)

                    _ ->
                        Json.Decode.succeed (ReadError UnknownError)
            )
