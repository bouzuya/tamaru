module Server.Route
  ( Action(..)
  , GroupIdLike
  , DataIdLike
  , route
  ) where

import Prelude

import Bouzuya.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))

type GroupIdLike = String
type DataIdLike = String

data Action
  = GetIndex
  | GetGroupList
  | GetGroup GroupIdLike
  | GetGroupDataList GroupIdLike
  | CreateGroupData GroupIdLike
  | GetGroupData GroupIdLike DataIdLike

instance showAction :: Show Action where
  show GetIndex = "GetIndex"
  show GetGroupList = "GetGroupList"
  show (GetGroup groupId) = "GetGroup(" <> groupId <> ")"
  show (GetGroupDataList groupId) = "GetGroupDataList(" <> groupId <> ")"
  show (CreateGroupData groupId) = "CreateGroupData(" <> groupId <> ")"
  show (GetGroupData groupId dataId)
    = "GetGroupData(" <> groupId <> "," <> dataId <> ")"

route :: Method -> Array String -> Maybe Action
route GET [] = Just GetIndex
route GET ["groups"] = Just GetGroupList
route GET ["groups", groupId] = Just (GetGroup groupId)
route GET ["groups", groupId, "data"] = Just (GetGroupDataList groupId)
route POST ["groups", groupId, "data"] = Just (CreateGroupData groupId)
route GET ["groups", groupId, "data", dataId]
  = Just (GetGroupData groupId dataId)
route _ _ = Nothing
