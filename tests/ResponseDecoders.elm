module ResponseDecoders exposing (..)

import AppRoutes exposing (Route)
import Data.Response as Response exposing (decodeResponse)
import Expect
import Json.Decode
import Test exposing (Test, describe, test)


xhrResponseTests : Test
xhrResponseTests =
    describe "Decoder test"
        [ test "Decodes an xhr response" <|
            \() ->
                let
                    decodedOutput =
                        Json.Decode.decodeString
                            (decodeResponse "https://api.dev.nexosisdev.com/v1")
                            xhrResponseJson
                in
                case decodedOutput of
                    Ok result ->
                        Expect.equal
                            [ { message = "Nexosis could not execute any algorithms to model this dataset. The most common cause of this error is that the dataset does not contain enough rows prior to the forecast start date to adequately train and test a model. Please make sure that the dataset contains at least 150 records prior to the forecast start date and try the request again."
                              , severity = Response.Informational
                              , routeToResource = Just (AppRoutes.SessionDetail "015ece43-87b5-4edb-b2b8-15ca957333fe")
                              }
                            , { message = "Nexosis could not execute any algorithms to model this dataset. The most common cause of this error is that the dataset does not contain enough rows prior to the forecast start date to adequately train and test a model. Please make sure that the dataset contains at least 150 records prior to the forecast start date and try the request again."
                              , severity = Response.Error
                              , routeToResource = Just (AppRoutes.SessionDetail "015ece43-87b5-4edb-b2b8-15ca957333fe")
                              }
                            , { message = "Usage Alert - You have 0 Predictions left for this cycle."
                              , severity = Response.Error
                              , routeToResource = Just (AppRoutes.SessionDetail "015ece43-87b5-4edb-b2b8-15ca957333fe")
                              }
                            ]
                            result.messages

                    Err err ->
                        Expect.fail err
        , test "Decodes missing messages object" <|
            \() ->
                let
                    decodedOutput =
                        Json.Decode.decodeString
                            (decodeResponse "https://api.dev.nexosisdev.com/v1")
                            xhrNoMessages
                in
                case decodedOutput of
                    Ok result ->
                        Expect.equal
                            []
                            result.messages

                    Err err ->
                        Expect.fail err
        ]


xhrResponseJson : String
xhrResponseJson =
    """{"status":200,"statusText":"OK","response":"{\\"sessionId\\": \\"015ece43-87b5-4edb-b2b8-15ca957333fe\\", \\"type\\": \\"forecast\\", \\"status\\": \\"failed\\", \\"predictionDomain\\": \\"forecast\\", \\"messages\\": [ { \\"severity\\": \\"informational\\", \\"message\\": \\"Nexosis could not execute any algorithms to model this dataset. The most common cause of this error is that the dataset does not contain enough rows prior to the forecast start date to adequately train and test a model. Please make sure that the dataset contains at least 150 records prior to the forecast start date and try the request again.\\" }, { \\"severity\\": \\"error\\", \\"message\\": \\"Nexosis could not execute any algorithms to model this dataset. The most common cause of this error is that the dataset does not contain enough rows prior to the forecast start date to adequately train and test a model. Please make sure that the dataset contains at least 150 records prior to the forecast start date and try the request again.\\"},{ \\"severity\\": \\"error\\", \\"message\\": \\"Usage Alert - You have 0 Predictions left for this cycle.\\" }]}","method":"GET","url":"https://api.dev.nexosisdev.com/v1/sessions/015ece43-87b5-4edb-b2b8-15ca957333fe","timestamp":"2018-02-07T19:53:35.698Z"}"""


xhrNoMessages : String
xhrNoMessages =
    """{"status":200,"statusText":"OK","response":"{\\"sessionId\\": \\"015ece43-87b5-4edb-b2b8-15ca957333fe\\", \\"type\\": \\"forecast\\", \\"status\\": \\"failed\\", \\"predictionDomain\\": \\"forecast\\"}","method":"GET","url":"https://api.dev.nexosisdev.com/v1/sessions/015ece43-87b5-4edb-b2b8-15ca957333fe","timestamp":"2018-02-07T19:53:35.698Z"}"""
