module Data.File exposing (FileReadStatus(..), FileUploadErrorType(..), JsonData, batchCsvData, batchJsonData, fileReadStatusDecoder, jsonDataDecoder, jsonDataEncoder)

import Csv
import Dict exposing (Dict)
import Json.Decode exposing (dict, float, int, keyValuePairs, list, map, oneOf, string)
import Json.Decode.Pipeline exposing (custom, decode, optional, required)
import Json.Encode
import Json.Encode.Extra
import List exposing (drop, take)
import Nexosis.Decoders.Columns as Columns exposing (decodeColumnMetadata)
import Nexosis.Encoders.Columns exposing (encodeColumnMetadataList)
import Nexosis.Types.Columns exposing (ColumnMetadata)


type FileUploadErrorType
    = FileTooLarge
    | UnsupportedFileType
    | UnknownError


type FileReadStatus
    = ReadError FileUploadErrorType
    | Success String String


type alias JsonData =
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


batchJsonData : Int -> (JsonData -> a) -> JsonData -> List a
batchJsonData batchSize callBack data =
    let
        newData batch =
            JsonData
                data.columns
                batch

        process : List (Dict String String) -> a
        process batch =
            newData batch |> callBack
    in
    List.map process <| split batchSize data.data


batchCsvData : Int -> (Csv.Csv -> a) -> Csv.Csv -> List a
batchCsvData batchSize callBack data =
    let
        newData batch =
            Csv.Csv
                data.headers
                batch

        process : List (List String) -> a
        process batch =
            newData batch |> callBack
    in
    List.map process <| split batchSize data.records


jsonDataDecoder : Json.Decode.Decoder JsonData
jsonDataDecoder =
    let
        val =
            oneOf [ string, map toString int, map toString float ]

        row =
            dict val
    in
    decode JsonData
        |> optional "columns" decodeColumnMetadata []
        |> required "data" (list row)


jsonDataEncoder : JsonData -> Json.Encode.Value
jsonDataEncoder data =
    Json.Encode.object
        [ ( "columns", encodeColumnMetadataList data.columns )
        , ( "data", Json.Encode.list (List.map (Json.Encode.Extra.dict identity Json.Encode.string) data.data) )
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
