module Feature exposing (Feature(..), isEnabled)


type Feature
    = Imports
    | Sessions
    | DataSets
    | Models


isEnabled : List Feature -> Feature -> Bool
isEnabled enabledFeatures feature =
    List.member feature enabledFeatures
