module Request.Model exposing (get)

import Data.Config as Config exposing (Config, withAuthorization)
import Data.Model exposing (ModelList, decodeModelList)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)


get : Config -> Int -> Http.Request ModelList
get { baseUrl, token } page =
    let
        expect =
            decodeModelList
                |> Http.expectJson

        params =
            pageParams page Config.pageSize
    in
    (baseUrl ++ "/models")
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
