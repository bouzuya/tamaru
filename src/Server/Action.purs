module Server.Action
  ( handleAction
  ) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server.Node (ServerEff)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Prelude (bind, pure, ($))
import Server.DB (db, findDataAllByGroupId, findDataByGroupIdAndDataId, findGroupAll, findGroupById)
import Server.Response (response200, response404)
import Server.Route (Action(..))
import Server.View (View(..))

handleAction :: forall e. Action -> Request -> Aff (ServerEff e) Response
handleAction GetGroupList _ = do
  groups <- pure $ findGroupAll db
  view <- pure $ GroupListView groups
  pure $ response200 view
handleAction (GetGroup groupId) _ = do
  groupMaybe <- pure $ findGroupById db groupId
  case groupMaybe of
    Nothing ->
      pure response404
    Just group -> do
      view <- pure $ GroupView group
      pure $ response200 view
handleAction (GetGroupDataList groupId) _ = do
  allDataMaybe <- pure $ findDataAllByGroupId db groupId
  case allDataMaybe of
    Nothing ->
      pure response404
    Just allData -> do
      view <- pure $ DataListView allData
      pure $ response200 view
handleAction (GetGroupData groupId dataId) _ = do
  dataMaybe <- pure $ findDataByGroupIdAndDataId db groupId dataId
  case dataMaybe of
    Nothing ->
      pure response404
    Just d -> do
      view <- pure $ DataView d
      pure $ response200 view
handleAction action request = do
  view <- pure $ RequestView request
  pure $ response200 view
