module Data.Context exposing (..)

import Data.Config exposing (Config)
import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, required)
import Json.Encode as Encode exposing (object)


type alias ContextModel =
    { userPageSize : Int
    , config : Config
    }


setPageSize : ContextModel -> Int -> ContextModel
setPageSize model pageSize =
    { model | userPageSize = pageSize }


encode : ContextModel -> Encode.Value
encode ctx =
    object
        [ ( "userPageSize", Encode.int <| ctx.userPageSize )
        ]


modelDecoder : Decoder ContextModel
modelDecoder =
    decode ContextModel
        |> required "userPageSize" Decode.int
        |> hardcoded (Config "" Nothing "" "" "" Dict.empty)


defaultContext : ContextModel
defaultContext =
    ContextModel 10 (Config "" Nothing "" "" "" Dict.empty)
