module Data.ImputationStrategy exposing (ImputationStrategy(..), enumImputationStrategy, stringToImputationStrategy)


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


stringToImputationStrategy : String -> Result String ImputationStrategy
stringToImputationStrategy input =
    case String.toLower input of
        "zeroes" ->
            Ok Zeroes

        "mean" ->
            Ok Mean

        "median" ->
            Ok Median

        "mode" ->
            Ok Mode

        "min" ->
            Ok Min

        "max" ->
            Ok Max

        _ ->
            Err "unknown imputation strategy provided"
