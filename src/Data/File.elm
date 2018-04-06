module Data.File exposing (FileReadStatus(..), FileUploadErrorType(..), JsonFile, batchJsonFile, fileReadStatusDecoder, jsonFileDecoder, jsonFileEncoder)

import Data.Columns as Columns exposing (ColumnMetadata, decodeColumnMetadata, encodeColumnMetadataList)
import Dict exposing (Dict)
import Json.Decode exposing (dict, keyValuePairs, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Encode
import Json.Encode.Extra
import List exposing (drop, take)


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


split : Int -> List a -> List (List a)
split i list =
    case take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (drop i list)


batchJsonFile : Int -> (JsonFile -> a) -> JsonFile -> List a
batchJsonFile batchSize callBack file =
    let
        newFile batch =
            JsonFile
                file.columns
                batch

        process : List (Dict String String) -> a
        process batch =
            newFile batch |> callBack
    in
    List.map process <| split batchSize file.data


jsonFileDecoder : Json.Decode.Decoder JsonFile
jsonFileDecoder =
    let
        row =
            dict string
    in
    decode JsonFile
        |> optional "columns" decodeColumnMetadata []
        |> required "data" (list row)


jsonFileEncoder : JsonFile -> Json.Encode.Value
jsonFileEncoder file =
    Json.Encode.object
        [ ( "columns", encodeColumnMetadataList file.columns )
        , ( "data", Json.Encode.list (List.map (Json.Encode.Extra.dict identity Json.Encode.string) file.data) )
        ]


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
