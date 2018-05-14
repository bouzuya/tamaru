module Bouzuya.HTTP.Response
  ( Response
  ) where

import Bouzuya.HTTP.Header (Header)
import Bouzuya.HTTP.StatusCode (StatusCode)

type Response =
  { body :: String
  , headers :: Array Header
  , status :: StatusCode
  }
