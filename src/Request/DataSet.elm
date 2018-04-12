module Request.DataSet exposing (batch)

import Data.Config exposing (Config)
import Data.File as File
import Http


batch : PutUploadRequest -> (( String, String, String ) -> a) -> List a
batch request put =
    case request.data of
        Json json ->
            let
                jsonDataToString data =
                    Encode.encode 0 (File.jsonDataEncoder data)

                upload data =
                    put ( request.name, jsonDataToString data, dataFormatToContentType DataFormat.Json )
            in
            File.batchJsonData 2000 upload <| json

        Csv csv ->
            let
                csvDataToString data =
                    let
                        sep =
                            String.join ","

                        header =
                            sep data.headers

                        line =
                            String.join "\x0D\n"
                    in
                    line <| [ header ] ++ List.map sep data.records

                upload data =
                    put ( request.name, csvDataToString data, dataFormatToContentType DataFormat.Csv )
            in
            File.batchCsvData 3000 upload <| csv


putInternal : Config -> ( String, String, String ) -> Http.Request ()
putInternal config ( name, content, contentType ) =
    (getBaseUrl config.clientConfig ++ "/data/" ++ Http.encodeUri name)
        |> HttpBuilder.put
        |> HttpBuilder.withBody (Http.stringBody contentType content)
        |> withAuthorization config.clientConfig
        |> withAppHeader config.clientConfig
        |> HttpBuilder.toRequest
