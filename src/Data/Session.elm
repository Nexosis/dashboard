module Data.Session exposing (..)

import Data.Columns exposing (ColumnMetadata, decodeColumnMetadata)
import Data.PredictionDomain exposing(..)

type SessionStatus = 
  Requested | Started | Completed

type alias SessionData = {
  sessionId : String
  ,columns : ColumnMetadata
  ,status :  SessionStatus
  ,predictionDomain : PredictionDomain
}