module Request.Session exposing (..)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.Session exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)


get : Config -> Int -> Int -> Http.Request SessionList
get { baseUrl, token } page pageSize =
    let
        expect =
            decodeSessionList
                |> Http.expectJson

        params =
            pageParams page pageSize
    in
    (baseUrl ++ "/sessions")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


results : Config -> String -> Int -> Int -> Http.Request SessionResults
results { baseUrl, token } sessionId page pageSize =
    let
        expect =
            decodeSessionResults
                |> Http.expectJson

        params =
            pageParams page pageSize
    in
    (baseUrl ++ "/sessions/" ++ sessionId ++ "/results")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withQueryParams params
        |> withAuthorization token
        |> HttpBuilder.toRequest


pageParams : Int -> Int -> List ( String, String )
pageParams page pageSize =
    [ ( "page", page |> toString )
    , ( "pageSize", pageSize |> toString )
    ]


delete : Config -> String -> Http.Request ()
delete { baseUrl, token } sessionId =
    (baseUrl ++ "/sessions/" ++ sessionId)
        |> HttpBuilder.delete
        |> withAuthorization token
        |> HttpBuilder.toRequest


getOne : Config -> String -> Http.Request SessionData
getOne { baseUrl, token } sessionId =
    let
        expect =
            decodeSession
                |> Http.expectJson
    in
    (baseUrl ++ "/sessions/" ++ sessionId)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization token
        |> HttpBuilder.toRequest
