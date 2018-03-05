module Data.Status exposing (HistoryRecord, Status(..), decodeHistoryRecord, decodeStatus)

import Data.DisplayDate exposing (dateDecoder)
import Json.Decode exposing (Decoder, andThen, fail, field, map2, string, succeed)
import Time.ZonedDateTime exposing (ZonedDateTime)


type Status
    = Requested
    | Started
    | Completed
    | Cancelled
    | Failed
    | CancellationPending


type alias HistoryRecord =
    { date : ZonedDateTime
    , status : Status
    }


decodeHistoryRecord : Decoder HistoryRecord
decodeHistoryRecord =
    map2 HistoryRecord
        (field "date" dateDecoder)
        (field "status" decodeStatus)


decodeStatus : Decoder Status
decodeStatus =
    string
        |> andThen
            (\n ->
                case n of
                    "requested" ->
                        succeed Requested

                    "started" ->
                        succeed Started

                    "completed" ->
                        succeed Completed

                    "cancelled" ->
                        succeed Cancelled

                    "failed" ->
                        succeed Failed

                    "cancellationpending" ->
                        succeed CancellationPending

                    unknown ->
                        fail <| "Unknown session status: " ++ unknown
            )
