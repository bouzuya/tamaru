module Server.Route
  ( route
  ) where

import Bouzuya.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Server.Action (Action(..))

route :: Method -> Array String -> Maybe Action
route GET [] = Just GetIndex
route GET ["groups"] = Just GetGroupList
route GET ["groups", groupId] = Just (GetGroup groupId)
route GET ["groups", groupId, "data"] = Just (GetGroupDataList groupId)
route POST ["groups", groupId, "data"] = Just (CreateGroupData groupId)
route GET ["groups", groupId, "data", dataId]
  = Just (GetGroupData groupId dataId)
route _ _ = Nothing
