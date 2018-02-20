module Data.ImputationStrategy exposing (ImputationStrategy(..))


type ImputationStrategy
    = Zeroes
    | Mean
    | Median
    | Mode
    | Min
    | Max
