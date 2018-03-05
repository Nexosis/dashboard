module Feature exposing (Feature(..), featureDecoder, isEnabled)

import Json.Decode exposing (Decoder, andThen, fail, string, succeed)


type Feature
    = PutSomeFeaturesHere


isEnabled : List Feature -> Feature -> Bool
isEnabled enabledFeatures feature =
    List.member feature enabledFeatures


featureDecoder : Decoder Feature
featureDecoder =
    string
        |> andThen
            (\featureName ->
                case featureName of
                    "PutSomeFeaturesHere" ->
                        succeed PutSomeFeaturesHere

                    unknown ->
                        fail <| "Unknown feature flag set: " ++ unknown
            )
