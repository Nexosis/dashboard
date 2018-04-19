module Data.Context exposing (..)

import Data.Config exposing (Config)
import Data.Response exposing (Quotas)
import Json.Decode as Decode exposing (Decoder, andThen, field, int, maybe, oneOf, string, succeed)
import Json.Decode.Extra exposing (doubleEncoded)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, required)
import Json.Encode as Encode exposing (object)
import Jwt
import Nexosis exposing (ClientConfig, createConfigWithApiKeyOptions, createConfigWithTokenOptions)
import Nexosis.Api.Metrics exposing (Metric)


type alias ContextModel =
    { config : Config
    , metricExplainers : List Metric
    , quotas : Maybe Quotas
    , auth : UserAuth
    , localStorage : LocalStorageState
    }


type alias LocalStorageState =
    { userPageSize : Int
    }


type UserAuth
    = KeyAuth Nexosis.ClientConfig
    | TokenAuth Nexosis.ClientConfig NexosisToken IdentityToken


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


contextToAuth : ContextModel -> ClientConfig
contextToAuth context =
    case context.auth of
        KeyAuth config ->
            config

        TokenAuth config _ _ ->
            config


getBaseUrl : ContextModel -> String
getBaseUrl context =
    case context.auth of
        KeyAuth config ->
            Nexosis.getBaseUrl config

        TokenAuth config _ _ ->
            Nexosis.getBaseUrl config


getUserName : ContextModel -> Maybe String
getUserName context =
    case context.auth of
        KeyAuth _ ->
            Nothing

        TokenAuth _ _ idToken ->
            Just idToken.name


isTokenAuth : ContextModel -> Bool
isTokenAuth context =
    case context.auth of
        KeyAuth _ ->
            False

        TokenAuth _ _ _ ->
            True


getRawToken : ContextModel -> Maybe String
getRawToken context =
    case context.auth of
        KeyAuth _ ->
            Nothing

        TokenAuth _ nexosisToken _ ->
            Just nexosisToken.rawToken


setNewToken : ContextModel -> TokenResponse -> ContextModel
setNewToken context { accessToken, identityToken } =
    let
        baseUrl =
            getBaseUrl context

        newAuth =
            TokenAuth
                (createConfigWithTokenOptions
                    { token = accessToken.rawToken
                    , baseUrl = baseUrl
                    , applicationName = Just context.config.applicationName
                    }
                )
                accessToken
                identityToken
    in
    { context | auth = newAuth }


tokenResponseDecoder : Decoder TokenResponse
tokenResponseDecoder =
    decode TokenResponse
        |> custom tokenDecoder
        |> custom identityTokenDecoder


type alias ConfigOptions =
    { baseUrl : String
    , applicationName : Maybe String
    }


decodeUserAuth : Decoder UserAuth
decodeUserAuth =
    decodeConfigOptions
        |> andThen
            (\c ->
                oneOf
                    [ field "cookie" (tokenAuthDecoder c)
                    , field "apiKey" (keyAuthDecoder c)
                    ]
            )


tokenAuthDecoder : ConfigOptions -> Decoder UserAuth
tokenAuthDecoder config =
    doubleEncoded
        tokenResponseDecoder
        |> andThen
            (\{ accessToken, identityToken } ->
                succeed <|
                    TokenAuth
                        (createConfigWithTokenOptions
                            { token = accessToken.rawToken
                            , baseUrl = config.baseUrl
                            , applicationName = config.applicationName
                            }
                        )
                        accessToken
                        identityToken
            )


keyAuthDecoder : ConfigOptions -> Decoder UserAuth
keyAuthDecoder config =
    string
        |> andThen
            (\key ->
                succeed <|
                    KeyAuth
                        (createConfigWithApiKeyOptions
                            { apiKey = key
                            , baseUrl = config.baseUrl
                            , applicationName = config.applicationName
                            }
                        )
            )


decodeConfigOptions : Decoder ConfigOptions
decodeConfigOptions =
    decode ConfigOptions
        |> required "apiUrl" string
        |> custom (field "applicationName" string |> andThen (\a -> succeed (Just a)))



-- |> custom (maybe tokenDecoder)
-- |> custom (decodeConfigOptions |> andThen (\options -> succeed (createConfigWithTokenOptions options)))
-- |> custom (maybe identityTokenDecoder)


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


setPageSize : ContextModel -> Int -> LocalStorageState
setPageSize context pageSize =
    let
        ls =
            context.localStorage
    in
    { ls | userPageSize = pageSize }


encode : LocalStorageState -> Encode.Value
encode ls =
    object
        [ ( "userPageSize", Encode.int <| ls.userPageSize ) ]


localStorageDecoder : Decoder LocalStorageState
localStorageDecoder =
    decode LocalStorageState
        |> required "userPageSize" Decode.int


defaultLocalStorageState : LocalStorageState
defaultLocalStorageState =
    LocalStorageState 10
