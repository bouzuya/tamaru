module Server.HTTP.Status
  ( Status(..)
  , StatusCode
  , StatusMessage
  ) where

type StatusCode = Int
type StatusMessage = String
data Status = Status StatusCode StatusMessage
