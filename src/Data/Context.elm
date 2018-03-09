module Data.Context exposing (..)

import Data.Config exposing (Config)


type alias ContextModel =
    { defaultPageSize : Int
    , config : Config
    }
