module Bouzuya.HTTP.Response
  ( Response
  ) where

import Bouzuya.HTTP.Header (Header)
import Bouzuya.HTTP.StatusCode (StatusCode)
import Data.ArrayBuffer.Types (Uint8Array)

type Response =
  { body :: Uint8Array
  , headers :: Array Header
  , status :: StatusCode
  }
