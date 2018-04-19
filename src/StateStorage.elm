-- From https://github.com/flq/elmorse


module StateStorage exposing (Msg(..), appStateLoaded, loadAppState, saveAppState, updateContext)

import Data.Config exposing (Config)
import Data.Context exposing (ContextModel, LocalStorageState, defaultLocalStorageState, encode, localStorageDecoder)
import Json.Decode as Decode exposing (Decoder, float, int, list, string)
import Ports exposing (objectRetrieved, retrieveObject, storeObject)


type Msg
    = OnAppStateLoaded LocalStorageState


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
    { model | context = newContext }


appStateLoaded : Sub Msg
appStateLoaded =
    let
        getModel json =
            case Decode.decodeValue localStorageDecoder json of
                Ok m ->
                    m

                Err _ ->
                    defaultLocalStorageState

        retrieval ( key, json ) =
            OnAppStateLoaded (getModel json)
    in
    objectRetrieved retrieval


saveAppState : LocalStorageState -> Cmd msg
saveAppState model =
    storeObject ( stateKey, encode model )
