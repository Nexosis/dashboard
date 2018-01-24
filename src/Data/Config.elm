module Data.Config exposing (Config, ApiKey(..), attempt, withAuthorization, decodeApiKey)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Value)
import Util exposing ((=>))


type alias Config =
    { apiKey : ApiKey
    , baseUrl : String
    , pageSize : Int
    }


type ApiKey
    = ApiKey String


decodeApiKey : Decode.Decoder ApiKey
decodeApiKey =
    Decode.string
        |> Decode.map ApiKey


attempt : String -> (ApiKey -> Cmd msg) -> Config -> ( List String, Cmd msg )
attempt attemptedAction toCmd config =
    -- case Maybe.map .key config of
    --     Nothing ->
    --         [ "You are signed out."] => Cmd.none
    --     Just key ->
    [] => toCmd config.apiKey


withAuthorization : Maybe ApiKey -> RequestBuilder a -> RequestBuilder a
withAuthorization maybeKey builder =
    case maybeKey of
        Just (ApiKey key) ->
            builder
                |> withHeader "api-key" key

        Nothing ->
            builder
