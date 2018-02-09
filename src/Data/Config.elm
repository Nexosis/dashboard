module Data.Config exposing (Config, configDecoder, pageSize, withAuthorization)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (andThen, string)
import Json.Decode.Pipeline as Pipeline
import Jwt


type alias Config =
    { baseUrl : String
    , token : Maybe NexosisToken
    }


type alias NexosisToken =
    { iat : Int
    , exp : Int
    , userId : Maybe String
    , email : Maybe String
    , rawToken : String
    }


configDecoder : Decode.Decoder Config
configDecoder =
    Pipeline.decode Config
        |> Pipeline.required "url" Decode.string
        |> Pipeline.custom
            (Decode.nullable
                (Decode.field "token"
                    string
                    |> andThen (\t -> Jwt.tokenDecoder (nexosisTokenDecoder t))
                )
            )


nexosisTokenDecoder : String -> Decode.Decoder NexosisToken
nexosisTokenDecoder rawToken =
    Pipeline.decode NexosisToken
        |> Pipeline.required "iat" Decode.int
        |> Pipeline.required "exp" Decode.int
        |> Pipeline.optional "GivenName" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "Email" (Decode.nullable Decode.string) Nothing
        |> Pipeline.hardcoded rawToken



-- attempt : String -> (String -> Cmd msg) -> Config -> ( List String, Cmd msg )
-- attempt attemptedAction toCmd config =
--     [] => toCmd config.rawToken


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
