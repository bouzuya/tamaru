module Server.HTTP.StatusCode
  ( StatusCode(..)
  , Code
  , ReasonPhrase
  ) where

type Code = Int
type ReasonPhrase = String
data StatusCode = StatusCode Code ReasonPhrase
