module Data.DataFormat exposing (DataFormat(..), dataFormatToContentType, dataFormatToString, filenameToType, parseDataFormat)


type DataFormat
    = Json
    | Csv
    | Other


parseDataFormat : String -> DataFormat
parseDataFormat input =
    case String.toLower input of
        "json" ->
            Json

        "csv" ->
            Csv

        _ ->
            Other


dataFormatToString : DataFormat -> String
dataFormatToString format =
    case format of
        Json ->
            "json"

        Csv ->
            "csv"

        _ ->
            "other"


filenameToType : String -> DataFormat
filenameToType name =
    let
        lowerString =
            String.toLower name
    in
    if String.endsWith ".json" lowerString then
        Json
    else if String.endsWith ".csv" lowerString then
        Csv
    else
        Other


dataFormatToContentType : DataFormat -> String
dataFormatToContentType uploadType =
    case uploadType of
        Json ->
            "application/json"

        Csv ->
            "text/csv"

        _ ->
            ""
