-- From https://raw.githubusercontent.com/flq/elmorse/


module StateStorage exposing (Msg, appStateLoaded, loadAppState, saveAppState)

import Data.Context exposing (ContextModel)
import Interop exposing (objectRetrieved, retrieveObject, storeObject)
import Json.Decode as Decode exposing (Decoder, float, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (object)


type Msg
    = OnAppStateLoaded (Maybe ContextModel)


stateKey : String
stateKey =
    "appState"


loadAppState : Cmd msg
loadAppState =
    retrieveObject stateKey


appStateLoaded : Sub Msg
appStateLoaded =
    let
        getModel json =
            case Decode.decodeValue modelDecoder json of
                Ok m ->
                    Just m

                Err _ ->
                    Nothing

        retrieval ( key, json ) =
            OnAppStateLoaded (getModel json)
    in
    objectRetrieved retrieval


saveAppState : ContextModel -> Cmd msg
saveAppState model =
    let
        map m =
            { defaultPageSize = m.defaultPageSize
            }
    in
    storeObject ( stateKey, encode <| map model )


encode : ContextModel -> Encode.Value
encode ctx =
    object
        [ ( "defaultPageSize", Encode.int ctx.defaultPageSize )
        ]


modelDecoder : Decoder ContextModel
modelDecoder =
    decode ContextModel
        |> required "defaultPageSize" int
