module Data.File exposing (FileReadStatus(..), dataFormatToContentType, fileReadStatusDecoder, filenameToType)

import Data.DataFormat as DataFormat
import Json.Decode


type FileReadStatus
    = ReadError
    | Success String String


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

                    _ ->
                        Json.Decode.succeed ReadError
            )


filenameToType : String -> DataFormat.DataFormat
filenameToType name =
    let
        lowerString =
            String.toLower name
    in
    if String.endsWith ".json" lowerString then
        DataFormat.Json
    else if String.endsWith ".csv" lowerString then
        DataFormat.Csv
    else
        DataFormat.Other


dataFormatToContentType : DataFormat.DataFormat -> String
dataFormatToContentType uploadType =
    case uploadType of
        DataFormat.Json ->
            "application/json"

        DataFormat.Csv ->
            "text/csv"

        _ ->
            ""
