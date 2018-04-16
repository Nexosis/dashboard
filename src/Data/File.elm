module Data.File exposing (CsvData, FileReadStatus(..), FileUploadErrorType(..), JsonData, batchCsvData, batchJsonData, calculateCsvBatchSize, calculateJsonBatchSize, fileReadStatusDecoder, jsonDataDecoder, jsonDataEncoder)

import Data.Columns as Columns exposing (ColumnMetadata, decodeColumnMetadata, encodeColumnMetadataList)
import Dict exposing (Dict)
import Json.Decode exposing (dict, float, int, keyValuePairs, list, map, oneOf, string)
import Json.Decode.Pipeline exposing (custom, decode, optional, required)
import Json.Encode
import Json.Encode.Extra
import List exposing (drop, take)
import List.Extra exposing (maximumBy)


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


type alias CsvData =
    { headers : List String
    , records : List String
    }


split : Int -> List a -> List (List a)
split i list =
    case take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (drop i list)


calculateCsvBatchSize : CsvData -> Int
calculateCsvBatchSize csv =
    let
        sizeOf row =
            String.length row

        rowSize records =
            let
                maxSize =
                    maximumBy sizeOf records
            in
            case maxSize of
                Nothing ->
                    1

                Just record ->
                    sizeOf record

        batchSize rowSize =
            floor <| ((1024 * 1024 * 0.75) / toFloat rowSize)
    in
    csv.records
        |> rowSize
        |> batchSize


calculateJsonBatchSize : JsonData -> Int
calculateJsonBatchSize json =
    let
        sizeOf row =
            Json.Encode.encode 0 (jsonDataEncoder (JsonData [] [ row ]))
                |> String.length

        rowSize json =
            let
                maxSize =
                    maximumBy sizeOf <| List.take 100 json.data
            in
            case maxSize of
                Nothing ->
                    1

                Just r ->
                    sizeOf r

        batchSize rowSize =
            floor <| ((1024 * 1024 * 0.75) / toFloat rowSize)
    in
    json
        |> rowSize
        |> batchSize


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


batchCsvData : Int -> (CsvData -> a) -> CsvData -> List a
batchCsvData batchSize callBack data =
    let
        newData batch =
            CsvData
                data.headers
                batch

        process : List String -> a
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
