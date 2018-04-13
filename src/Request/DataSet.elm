module Request.DataSet exposing (batchPut)

import Data.Config exposing (Config)
import Data.File as File exposing (UploadData(..), UploadDataRequest, encodeCsvDataFileUpload, encodeJsonDataFileUpload)
import Http
import Nexosis.Api.Data as Data


batch : UploadDataRequest -> (String -> String -> String -> a) -> List a
batch request put =
    case request.data of
        Json json ->
            let
                upload data =
                    let
                        ( content, contentType ) =
                            encodeJsonDataFileUpload json
                    in
                    put request.name content contentType
            in
            File.batchJsonData 2000 upload <| json

        Csv csv ->
            let
                upload data =
                    let
                        ( content, contentType ) =
                            encodeCsvDataFileUpload csv
                    in
                    put request.name content contentType
            in
            File.batchCsvData 3000 upload <| csv


batchPut : Config -> UploadDataRequest -> List (Http.Request ())
batchPut config request =
    batch request <| Data.put config.clientConfig
