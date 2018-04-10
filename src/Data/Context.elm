module Data.Context exposing (..)

import Data.Config exposing (Config)
import Data.Response exposing (Quotas)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, required)
import Json.Encode as Encode exposing (object)
import Nexosis.Metrics exposing (Metric)


type alias ContextModel =
    { userPageSize : Int
    , config : Config
    , metricExplainers : List Metric
    , quotas : Maybe Quotas
    }


setPageSize : ContextModel -> Int -> ContextModel
setPageSize model pageSize =
    { model | userPageSize = pageSize }


encode : ContextModel -> Encode.Value
encode ctx =
    object
        [ ( "userPageSize", Encode.int <| ctx.userPageSize )
        ]


modelDecoder : Config -> Decoder ContextModel
modelDecoder config =
    decode ContextModel
        |> required "userPageSize" Decode.int
        |> hardcoded config
        |> hardcoded []
        |> hardcoded Nothing


defaultContext : Config -> ContextModel
defaultContext config =
    ContextModel 10 config [] Nothing
