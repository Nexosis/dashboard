module Data.Config exposing (Config, NexosisToken, configDecoder, pageSize, tokenDecoder, withAuthorization)

import Dict exposing (Dict)
import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder, andThen, field, int, maybe, nullable, string)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, optional, required)
import Jwt


type alias Config =
    { baseUrl : String
    , token : Maybe NexosisToken
    , loginUrl : String
    , renewalUrl : String
    , toolTips : Dict String String
    }


type alias NexosisToken =
    { iat : Int
    , exp : Int
    , userId : Maybe String
    , email : Maybe String
    , rawToken : String
    }


configDecoder : Decoder Config
configDecoder =
    Pipeline.decode Config
        |> required "apiUrl" string
        |> custom
            (maybe
                tokenDecoder
            )
        |> required "loginUrl" string
        |> required "renewalUrl" string
        |> hardcoded Dict.empty


tokenDecoder : Decoder NexosisToken
tokenDecoder =
    field "token"
        string
        |> andThen (\t -> field "token" (Jwt.tokenDecoder (nexosisTokenDecoder t)))


nexosisTokenDecoder : String -> Decoder NexosisToken
nexosisTokenDecoder rawToken =
    decode NexosisToken
        |> required "iat" int
        |> required "exp" int
        |> optional "givenName" (nullable string) Nothing
        |> optional "email" (nullable string) Nothing
        |> hardcoded rawToken


withAuthorization : Maybe NexosisToken -> RequestBuilder a -> RequestBuilder a
withAuthorization token builder =
    case token of
        Just nexosisToken ->
            builder
                |> withHeader "Authorization" ("Bearer " ++ nexosisToken.rawToken)

        _ ->
            builder


pageSize : Int
pageSize =
    10
