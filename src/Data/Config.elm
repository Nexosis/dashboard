module Data.Config exposing (Config, NexosisToken, TokenResponse, configDecoder, tokenDecoder, tokenResponseDecoder, withAppHeader)

import Dict exposing (Dict)
import HttpBuilder exposing (RequestBuilder, withBearerToken, withHeader)
import Json.Decode as Decode exposing (Decoder, andThen, dict, field, int, maybe, nullable, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, optional, required)
import Jwt
import Nexosis exposing (ClientConfig, createConfigWithToken, withAuthorization)


type alias Config =
    { token : Maybe NexosisToken
    , clientConfig : ClientConfig
    , loginUrl : String
    , renewalUrl : String
    , subscriptionUrl : String
    , toolTips : Dict String String
    , explainerContent : Dict String String
    , applicationName : String
    , accountSiteUrl : String
    , apiManagerUrl : String
    , identityToken : Maybe IdentityToken
    }


type alias TokenResponse =
    { accessToken : NexosisToken
    , identityToken : IdentityToken
    }


type alias NexosisToken =
    { iat : Int
    , exp : Int
    , rawToken : String
    }


type alias IdentityToken =
    { name : String
    , email : String
    }


tokenResponseDecoder : Decoder TokenResponse
tokenResponseDecoder =
    Pipeline.decode TokenResponse
        |> custom tokenDecoder
        |> custom identityTokenDecoder


configDecoder : Decoder Config
configDecoder =
    Pipeline.decode Config
        |> custom (maybe tokenDecoder)
        |> custom decodeClientConfig
        |> required "loginUrl" string
        |> required "renewalUrl" string
        |> required "subscriptionUrl" string
        |> required "toolTips" toolTipDictDecoder
        |> required "explainerContent" (Decode.dict string)
        |> required "applicationName" string
        |> required "accountSite" string
        |> required "apiManagerUrl" string
        |> custom (maybe identityTokenDecoder)


decodeClientConfig : Decoder ClientConfig
decodeClientConfig =
    field "apiUrl" string
        |> andThen
            (\url ->
                field "token" string
                    |> andThen
                        (\token ->
                            succeed (createConfigWithToken url token)
                        )
            )


tokenDecoder : Decoder NexosisToken
tokenDecoder =
    field "token"
        string
        |> andThen (\t -> field "token" (Jwt.tokenDecoder (nexosisAccessTokenDecoder t)))


nexosisAccessTokenDecoder : String -> Decoder NexosisToken
nexosisAccessTokenDecoder rawToken =
    decode NexosisToken
        |> required "iat" int
        |> required "exp" int
        |> hardcoded rawToken


identityTokenDecoder : Decoder IdentityToken
identityTokenDecoder =
    field "identity" (Jwt.tokenDecoder nexosisIdentityTokenDecoder)


nexosisIdentityTokenDecoder : Decoder IdentityToken
nexosisIdentityTokenDecoder =
    decode IdentityToken
        |> required "name" string
        |> required "email" string


decodeDocs : Decoder (List String)
decodeDocs =
    Decode.list string


toolTipDictDecoder : Decoder (Dict String String)
toolTipDictDecoder =
    dict string


withAppHeader : Config -> RequestBuilder a -> RequestBuilder a
withAppHeader config builder =
    builder
        |> withHeader "application-name" config.applicationName
