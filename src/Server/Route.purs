module Server.Route
  ( route
  ) where

import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.Method as Method
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Server.Action (Action)
import Server.Action as Action

route :: Method -> Array String -> Maybe Action
route Method.GET [] =
  Maybe.Just Action.GetIndex
route Method.GET ["groups"] =
  Maybe.Just Action.GetGroupList
route Method.GET ["groups", groupId] =
  Maybe.Just (Action.GetGroup groupId)
route Method.GET ["groups", groupId, "data"] =
  Maybe.Just (Action.GetGroupDataList groupId)
route Method.PUT ["groups", groupId, "data"] =
  Maybe.Just (Action.UpdateGroupData groupId)
route Method.GET ["groups", groupId, "data", dataId] =
  Maybe.Just (Action.GetGroupData groupId dataId)
route _ _ =
  Maybe.Nothing
