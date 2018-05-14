module Bouzuya.HTTP.Request
  ( Request
  ) where

import Bouzuya.HTTP.Header (Header)
import Bouzuya.HTTP.Method (Method)
import Data.Tuple (Tuple)

type Request =
  { body :: String
  , headers :: Array Header
  , method :: Method
  , pathname :: String
  , searchParams :: Array (Tuple String String)
  }
