module Data.Config exposing (ApiKey, Config, attempt, decodeApiKey, decodeToken, pageSize, withAuthorization)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as Pipeline
import Jwt
import Jwt.Decoders
import Util exposing ((=>))


type alias Config =
    { apiKey : Maybe ApiKey
    , baseUrl : String
    , token : Maybe Jwt.Decoders.JwtToken
    , rawToken : String
    }


type ApiKey
    = ApiKey String


decodeApiKey : String -> Maybe ApiKey
decodeApiKey string =
    Just (ApiKey string)


apiKeyDecoder : Decode.Decoder ApiKey
apiKeyDecoder =
    Decode.string
        |> Decode.map ApiKey


decodeToken : String -> Maybe Jwt.Decoders.JwtToken
decodeToken stringToken =
    Jwt.decodeToken
        nexosisTokenDecoder
        stringToken
        |> Result.toMaybe


nexosisTokenDecoder : Decode.Decoder Jwt.Decoders.JwtToken
nexosisTokenDecoder =
    Pipeline.decode Jwt.Decoders.JwtToken
        |> Pipeline.required "iat" Decode.int
        |> Pipeline.required "exp" Decode.int
        |> Pipeline.optional "GivenName" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "Email" (Decode.nullable Decode.string) Nothing


attempt : String -> (ApiKey -> Cmd msg) -> Config -> ( List String, Cmd msg )
attempt attemptedAction toCmd config =
    case config.apiKey of
        Nothing ->
            [ "You are signed out." ] => Cmd.none

        Just key ->
            [] => toCmd key


withAuthorization : Maybe ApiKey -> RequestBuilder a -> RequestBuilder a
withAuthorization maybeKey builder =
    case maybeKey of
        Just (ApiKey key) ->
            builder
                |> withHeader "api-key" key

        Nothing ->
            builder


pageSize : Int
pageSize =
    10
