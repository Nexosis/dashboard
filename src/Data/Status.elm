module Data.Status exposing (HistoryRecord, Status(..), decodeHistoryRecord, decodeStatus)

import Json.Decode exposing (Decoder, andThen, fail, field, map2, string, succeed)


type Status
    = Requested
    | Started
    | Completed
    | Cancelled
    | Failed
    | CancellationPending


type alias HistoryRecord =
    { date : String
    , status : Status
    }


decodeHistoryRecord : Decoder HistoryRecord
decodeHistoryRecord =
    map2 HistoryRecord
        (field "date" string)
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
