module Data.Config exposing (Config, NexosisToken, configDecoder, tokenDecoder, withAuthorization)

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
    , navbarBaseUrl : String
    , identityToken : Maybe IdentityToken
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


configDecoder : Decoder Config
configDecoder =
    Pipeline.decode Config
        |> required "apiUrl" string
        |> custom (maybe tokenDecoder)
        |> required "loginUrl" string
        |> required "renewalUrl" string
        |> required "subscriptionUrl" string
        |> required "toolTips" toolTipDictDecoder
        |> required "explainerContent" (Decode.dict string)
        |> required "applicationName" string
        |> required "accountSite" string
        |> custom (maybe identityTokenDecoder)


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


withAuthorization : Config -> RequestBuilder a -> RequestBuilder a
withAuthorization config builder =
    case config.token of
        Just nexosisToken ->
            builder
                |> withBearerToken nexosisToken.rawToken
                |> withHeader "application-name" config.applicationName

        _ ->
            builder
