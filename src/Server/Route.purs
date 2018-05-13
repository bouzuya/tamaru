module Server.Route
  ( Action(..)
  , GroupId
  , DataId
  , route
  ) where

import Prelude

import Bouzuya.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))

type GroupId = String
type DataId = String

data Action
  = GetIndex
  | GetGroupList
  | GetGroup GroupId
  | GetGroupDataList GroupId
  | GetGroupData GroupId DataId

instance showAction :: Show Action where
  show GetIndex = "GetIndex"
  show GetGroupList = "GetGroupList"
  show (GetGroup groupId) = "GetGroup(" <> groupId <> ")"
  show (GetGroupDataList groupId) = "GetGroupDataList(" <> groupId <> ")"
  show (GetGroupData groupId dataId)
    = "GetGroupData(" <> groupId <> "," <> dataId <> ")"

route :: Method -> Array String -> Maybe Action
route GET [] = Just GetIndex
route GET ["groups"] = Just GetGroupList
route GET ["groups", groupId] = Just (GetGroup groupId)
route GET ["groups", groupId, "data"] = Just (GetGroupDataList groupId)
route GET ["groups", groupId, "data", dataId]
  = Just (GetGroupData groupId dataId)
route _ _ = Nothing
