module Data.ImputationStrategy exposing (enumImputationStrategy)

import Nexosis.Types.ImputationStrategy exposing (ImputationStrategy(..))


enumImputationStrategy : List ImputationStrategy
enumImputationStrategy =
    [ Zeroes
    , Mean
    , Median
    , Mode
    , Min
    , Max
    ]
