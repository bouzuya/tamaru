module Server.HTTP.StatusCode
  ( Code
  , ReasonPhrase
  , StatusCode(..)
  , status200
  ) where

type Code = Int
type ReasonPhrase = String
data StatusCode = StatusCode Code ReasonPhrase

status200 :: StatusCode
status200 = StatusCode 200 "OK"
