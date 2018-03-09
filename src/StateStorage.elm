-- From https://raw.githubusercontent.com/flq/elmorse/


module StateStorage exposing (Msg, appStateLoaded, loadAppState, modelDecoder, saveAppState, updateContext)

import Data.Config exposing (Config, configDecoder)
import Data.Context exposing (ContextModel)
import Interop exposing (objectRetrieved, retrieveObject, storeObject)
import Json.Decode as Decode exposing (Decoder, float, int, list, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode exposing (object)


type Msg
    = OnAppStateLoaded (Maybe ContextModel)


type alias ContextContainer a =
    { a
        | context : ContextModel
    }


stateKey : String
stateKey =
    "appState"


loadAppState : Cmd msg
loadAppState =
    retrieveObject stateKey


updateContext : ContextContainer a -> Maybe ContextModel -> ContextContainer a
updateContext model context =
    case context of
        Just ctx ->
            { model
                | context = ctx
            }

        Nothing ->
            model


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
            , config = m.config
            }
    in
    storeObject ( stateKey, encode <| map model )


encode : ContextModel -> Encode.Value
encode ctx =
    object
        [ ( "defaultPageSize", Encode.int <| ctx.defaultPageSize )
        ]


modelDecoder : Decoder ContextModel
modelDecoder =
    decode ContextModel
        |> hardcoded 10
        |> required "config" configDecoder
