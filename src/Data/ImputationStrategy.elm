module Data.ImputationStrategy exposing (ImputationStrategy(..), enumImputationStrategy)


type ImputationStrategy
    = Zeroes
    | Mean
    | Median
    | Mode
    | Min
    | Max


enumImputationStrategy : List ImputationStrategy
enumImputationStrategy =
    [ Zeroes
    , Mean
    , Median
    , Mode
    , Min
    , Max
    ]
