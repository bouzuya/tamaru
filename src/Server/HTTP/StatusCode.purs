module Server.HTTP.StatusCode
  ( Code
  , ReasonPhrase
  , StatusCode(..)
  , ResponseClass(..)
  , getResponseClass
  , isClientError
  , isInformational
  , isRedirection
  , isServerError
  , isSuccessful
  , status200
  ) where

import Data.Boolean (otherwise)
import Data.HeytingAlgebra ((&&))
import Data.Maybe (Maybe(..))
import Data.Ord ((<=))

type Code = Int
type ReasonPhrase = String
data StatusCode = StatusCode Code ReasonPhrase
data ResponseClass
  = Informational
  | Successful
  | Redirection
  | ClientError
  | ServerError

status200 :: StatusCode
status200 = StatusCode 200 "OK"

-- TODO: other statusXXX

getResponseClass :: StatusCode -> Maybe ResponseClass
getResponseClass (StatusCode code _)
  | 100 <= code && code <= 199 = Just Informational
  | 200 <= code && code <= 299 = Just Successful
  | 300 <= code && code <= 399 = Just Redirection
  | 400 <= code && code <= 499 = Just ClientError
  | 500 <= code && code <= 599 = Just ServerError
  | otherwise = Nothing

isInformational :: StatusCode -> Boolean
isInformational s = case getResponseClass s of
  Just Informational -> true
  _ -> false

isSuccessful :: StatusCode -> Boolean
isSuccessful s = case getResponseClass s of
  Just Successful -> true
  _ -> false

isRedirection :: StatusCode -> Boolean
isRedirection s = case getResponseClass s of
  Just Redirection -> true
  _ -> false

isClientError :: StatusCode -> Boolean
isClientError  s = case getResponseClass s of
  Just ClientError -> true
  _ -> false

isServerError :: StatusCode -> Boolean
isServerError s = case getResponseClass s of
  Just ServerError -> true
  _ -> false
