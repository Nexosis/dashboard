module Feature exposing (Feature(..), featureDecoder, isEnabled)

import Json.Decode exposing (Decoder, andThen, fail, string, succeed)


type Feature
    = Imports
    | Sessions
    | DataSets
    | Models


isEnabled : List Feature -> Feature -> Bool
isEnabled enabledFeatures feature =
    List.member feature enabledFeatures


featureDecoder : Decoder Feature
featureDecoder =
    string
        |> andThen
            (\featureName ->
                case featureName of
                    "Models" ->
                        succeed Models

                    "Sessions" ->
                        succeed Sessions

                    "DataSets" ->
                        succeed DataSets

                    "Imports" ->
                        succeed Imports

                    unknown ->
                        fail <| "Unknown feature flag set: " ++ unknown
            )
