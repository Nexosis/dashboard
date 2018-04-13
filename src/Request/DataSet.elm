module Request.DataSet exposing (batchPut)

import Data.Config exposing (Config)
import Data.File as File exposing (UploadData(..), UploadDataRequest, encodeCsvDataFileUpload, encodeJsonDataFileUpload)
import Http
import Nexosis.Api.Data as Data


batch : UploadDataRequest -> (String -> String -> String -> a) -> List a
batch request put =
    let
        calculateBatchSize request =
            case request.data of
                Json json ->
                    File.calculateJsonBatchSize json

                Csv csv ->
                    File.calculateCsvBatchSize csv

        batchSize =
            calculateBatchSize request
    in
    case request.data of
        Json json ->
            let
                upload data =
                    let
                        ( content, contentType ) =
                            encodeJsonDataFileUpload data
                    in
                    put request.name content contentType
            in
            File.batchJsonData batchSize upload <| json

        Csv csv ->
            let
                upload data =
                    let
                        ( content, contentType ) =
                            encodeCsvDataFileUpload data
                    in
                    put request.name content contentType
            in
            File.batchCsvData batchSize upload <| csv


batchPut : Config -> UploadDataRequest -> List (Http.Request ())
batchPut config request =
    batch request <| Data.put config.clientConfig
