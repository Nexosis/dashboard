module Data.File exposing (FileReadStatus(..), FileUploadErrorType(..), UploadData(..), UploadDataRequest, batchCsvData, batchJsonData, encodeCsvDataFileUpload, encodeJsonDataFileUpload, fileContentEncoder, fileReadStatusDecoder, jsonDataDecoder, parseJson)

import Csv
import Dict exposing (Dict)
import Json.Decode exposing (decodeString, dict, float, int, keyValuePairs, list, map, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, required)
import Json.Encode as Encode exposing (Value, object)
import Json.Encode.Extra
import List exposing (drop, take)
import Nexosis.Decoders.Columns as Columns exposing (decodeColumnMetadata)
import Nexosis.Types.Columns exposing (ColumnMetadata, DataType(..))
import Util exposing ((=>))


type alias UploadDataRequest =
    { name : String
    , data : UploadData
    }


type alias PredictionRequest =
    { modelId : String
    , data : UploadData
    }


type FileUploadErrorType
    = FileTooLarge
    | UnsupportedFileType
    | UnknownError


type FileReadStatus
    = ReadError FileUploadErrorType
    | Success String String


type UploadData
    = Json JsonData
    | Csv CsvData


type alias JsonData =
    { columns : List ColumnMetadata
    , data : List (Dict String String)
    }


type alias CsvData =
    { headers : List String
    , records : List (List String)
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


parseJson : field -> String -> Result (List ( field, String )) UploadData
parseJson field content =
    decodeString jsonDataDecoder content
        |> Result.map (\d -> Json d)
        |> Result.mapError (\e -> [ field => e ])


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


fileContentEncoder : { a | data : UploadData } -> ( String, String )
fileContentEncoder { data } =
    case data of
        Json json ->
            encodeJsonDataFileUpload json

        Csv csv ->
            encodeCsvDataFileUpload csv


encodeJsonDataFileUpload : JsonData -> ( String, String )
encodeJsonDataFileUpload json =
    ( Encode.encode 0 (jsonDataEncoder json), "application/json" )


encodeCsvDataFileUpload : CsvData -> ( String, String )
encodeCsvDataFileUpload csv =
    let
        sep =
            String.join ","

        header =
            sep csv.headers

        line =
            String.join "\x0D\n"

        encodedData =
            Encode.string <| line <| [ header ] ++ List.map sep csv.records
    in
    ( Encode.encode 0 encodedData, "text/csv" )


jsonDataEncoder : JsonData -> Encode.Value
jsonDataEncoder data =
    Encode.object
        [ ( "columns", encodeColumnMetadataList data.columns )
        , ( "data", Encode.list (List.map (Json.Encode.Extra.dict identity Encode.string) data.data) )
        ]


encodeColumnMetadataList : List ColumnMetadata -> Value
encodeColumnMetadataList columns =
    object <|
        (columns
            |> List.map (\c -> ( c.name, encodeColumnValues c ))
        )


encodeColumnValues : ColumnMetadata -> Value
encodeColumnValues column =
    object
        [ ( "dataType", encodeDataType <| column.dataType )
        , ( "role", Encode.string <| toString <| column.role )
        , ( "imputation", Encode.string <| toString <| column.imputation )
        , ( "aggregation", Encode.string <| toString <| column.aggregation )
        ]


encodeDataType : DataType -> Value
encodeDataType dataType =
    if dataType == Measure then
        Encode.string "numericMeasure"
    else
        Encode.string <| toString dataType
