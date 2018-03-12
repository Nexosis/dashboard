-- From https://github.com/flq/elmorse


module StateStorage exposing (Msg(..), appStateLoaded, loadAppState, saveAppState, updateContext)

import Data.Config exposing (Config, configDecoder)
import Data.Context exposing (ContextModel, defaultContext, encode, modelDecoder)
import Json.Decode as Decode exposing (Decoder, float, int, list, string)
import Ports exposing (objectRetrieved, retrieveObject, storeObject)


type Msg
    = OnAppStateLoaded ContextModel


type alias ContextContainer a =
    { a
        | context : ContextModel
        , config : Config
    }


stateKey : String
stateKey =
    "dashboardState"


loadAppState : Cmd msg
loadAppState =
    retrieveObject stateKey


updateContext : ContextContainer a -> ContextModel -> ContextContainer a
updateContext model context =
    let
        newContext =
            { context | config = model.config }
    in
    { model
        | context = newContext
    }


appStateLoaded : Sub Msg
appStateLoaded =
    let
        getModel json =
            case Decode.decodeValue modelDecoder json of
                Ok m ->
                    m

                Err _ ->
                    defaultContext

        retrieval ( key, json ) =
            OnAppStateLoaded (getModel json)
    in
    objectRetrieved retrieval


saveAppState : ContextModel -> Cmd msg
saveAppState model =
    -- let
    --     map m =
    --         { userPageSize = m.userPageSize
    --         , config = m.config
    --         }
    -- in
    storeObject ( stateKey, encode model )
