module Data.ImputationStrategy exposing (enumImputationStrategy)


enumImputationStrategy : List ImputationStrategy
enumImputationStrategy =
    [ Zeroes
    , Mean
    , Median
    , Mode
    , Min
    , Max
    ]
