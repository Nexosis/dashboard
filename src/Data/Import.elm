module Data.Import exposing (ImportDetail, decodeImportDetail)

import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Data.DataSet exposing (DataSetName, dataSetNameDecoder)
import Data.Message exposing (Message, decodeMessage)
import Data.Status exposing (HistoryRecord, Status, decodeHistoryRecord, decodeStatus)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue, dict, fail, field, float, int, list, map2, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Time.DateTime exposing (DateTime, fromISO8601)


type alias ImportDetail =
    { importId : String
    , importType : ImportType
    , status : Status
    , dataSetName : DataSetName
    , parameters : Dict String String
    , requestedDate : DateTime
    , statusHistory : List HistoryRecord
    , messages : List Message
    , columns : List ColumnMetadata
    }


type ImportType
    = S3
    | Url
    | Azure


decodeImportDetail : Decoder ImportDetail
decodeImportDetail =
    decode ImportDetail
        |> required "importId" string
        |> required "type" decodeImportType
        |> required "status" decodeStatus
        |> required "dataSetName" dataSetNameDecoder
        |> required "parameters" (dict string)
        |> required "requestedDate" decodeDate
        |> required "statusHistory" (list decodeHistoryRecord)
        |> required "messages" (list decodeMessage)
        |> required "columns" decodeColumnMetadata


decodeDate : Decoder DateTime
decodeDate =
    string
        |> andThen
            (\s ->
                case fromISO8601 s of
                    Ok date ->
                        succeed date

                    Err err ->
                        fail err
            )


decodeImportType : Decoder ImportType
decodeImportType =
    string
        |> andThen
            (\i ->
                case i of
                    "s3" ->
                        succeed S3

                    "url" ->
                        succeed Url

                    "Azure" ->
                        succeed Azure

                    unknown ->
                        fail <| "Unknown import type: " ++ unknown
            )
