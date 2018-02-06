module Data.Config exposing (ApiKey, Config, attempt, configDecoder, pageSize, withAuthorization)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as Pipeline
import Jwt
import Jwt.Decoders
import Util exposing ((=>))


type alias Config =
    { apiKey : ApiKey
    , baseUrl : String
    , token : Maybe Jwt.Decoders.JwtToken
    , rawToken : String
    }


type ApiKey
    = ApiKey String


configDecoder : Decode.Decoder Config
configDecoder =
    Pipeline.decode Config
        |> Pipeline.required "apiKey" apiKeyDecoder
        |> Pipeline.required "url" Decode.string
        |> Pipeline.custom (Decode.nullable (Decode.field "token" (Jwt.tokenDecoder nexosisTokenDecoder)))
        |> Pipeline.required "token" Decode.string


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
    [] => toCmd config.apiKey


withAuthorization : ApiKey -> RequestBuilder a -> RequestBuilder a
withAuthorization (ApiKey key) builder =
    builder
        |> withHeader "api-key" key


pageSize : Int
pageSize =
    10
