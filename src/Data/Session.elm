module Data.Session exposing (canPredictSession, sessionIsCompleted)

import Nexosis.Types.PredictionDomain exposing (PredictionDomain(..))
import Nexosis.Types.Session exposing (SessionData)
import Nexosis.Types.Status exposing (Status(..))


sessionIsCompleted : SessionData -> Bool
sessionIsCompleted session =
    case session.status of
        Completed ->
            True

        Failed ->
            True

        Cancelled ->
            True

        _ ->
            False


canPredictSession : SessionData -> Bool
canPredictSession session =
    case session.predictionDomain of
        Forecast ->
            False

        Impact ->
            False

        _ ->
            session.modelId /= Nothing
