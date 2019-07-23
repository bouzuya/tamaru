module Server.Action
  ( Action(..)
  , GroupIdLike
  , DataIdLike
  ) where

import Prelude

type GroupIdLike = String
type DataIdLike = String

data Action
  = GetIndex
  | GetGroupList
  | GetGroup GroupIdLike
  | GetGroupDataList GroupIdLike
  | UpdateGroupData GroupIdLike
  | GetGroupData GroupIdLike DataIdLike

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show GetIndex = "GetIndex"
  show GetGroupList = "GetGroupList"
  show (GetGroup groupId) = "GetGroup(" <> groupId <> ")"
  show (GetGroupDataList groupId) = "GetGroupDataList(" <> groupId <> ")"
  show (UpdateGroupData groupId) = "UpdateGroupData(" <> groupId <> ")"
  show (GetGroupData groupId dataId)
    = "GetGroupData(" <> groupId <> "," <> dataId <> ")"
