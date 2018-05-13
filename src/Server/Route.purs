module Server.Route
  ( Action(..)
  , route
  ) where

import Prelude

import Bouzuya.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))

data Action
  = GetIndex

instance showAction :: Show Action where
  show GetIndex = "GetIndex"

route :: Method -> Array String -> Maybe Action
route GET [] = Just GetIndex
route _ _ = Nothing
