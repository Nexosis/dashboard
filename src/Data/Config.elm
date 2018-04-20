module Data.Config exposing (Config, configDecoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, andThen, dict, field, int, maybe, nullable, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, optional, required)


type alias Config =
    { loginUrl : String
    , renewalUrl : String
    , subscriptionUrl : String
    , toolTips : Dict String String
    , explainerContent : Dict String String
    , applicationName : String
    , accountSiteUrl : String
    , apiManagerUrl : String
    }


configDecoder : Decoder Config
configDecoder =
    Pipeline.decode Config
        |> required "loginUrl" string
        |> required "renewalUrl" string
        |> required "subscriptionUrl" string
        |> required "toolTips" toolTipDictDecoder
        |> required "explainerContent" (Decode.dict string)
        |> required "applicationName" string
        |> required "accountSite" string
        |> required "apiManagerUrl" string


toolTipDictDecoder : Decoder (Dict String String)
toolTipDictDecoder =
    dict string
