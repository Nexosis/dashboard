module Data.Config exposing (Config, NexosisToken, configDecoder, pageSize, tokenDecoder, withAuthorization)

import Dict exposing (Dict)
import HttpBuilder exposing (RequestBuilder, withBearerToken, withHeader)
import Json.Decode as Decode exposing (Decoder, andThen, dict, field, int, maybe, nullable, string)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, optional, required)
import Jwt


type alias Config =
    { baseUrl : String
    , token : Maybe NexosisToken
    , loginUrl : String
    , renewalUrl : String
    , subscriptionUrl : String
    , toolTips : Dict String String
    , explainerContent : Dict String String
    , applicationName : String
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
        |> required "subscriptionUrl" string
        |> required "toolTips" toolTipDictDecoder
        |> required "explainerContent" (Decode.dict string)
        |> required "applicationName" string


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


decodeDocs : Decoder (List String)
decodeDocs =
    Decode.list string


toolTipDictDecoder : Decoder (Dict String String)
toolTipDictDecoder =
    dict string


withAuthorization : Config -> RequestBuilder a -> RequestBuilder a
withAuthorization config builder =
    case config.token of
        Just nexosisToken ->
            builder
                |> withBearerToken nexosisToken.rawToken
                |> withHeader "application-name" config.applicationName

        _ ->
            builder


pageSize : Int
pageSize =
    10
