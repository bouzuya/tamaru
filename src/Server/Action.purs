module Server.Action
  ( Action(..)
  , GroupIdLike
  , DataIdLike
  , handleAction
  ) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server.Node (ServerEff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (readVar)
import Data.Maybe (Maybe(..))
import Prelude (class Show, bind, pure, ($), (<>))
import Server.DB (Context, findDataAllByGroupId, findDataByGroupIdAndDataId, findGroupAll, findGroupById)
import Server.Response (response200, response404)
import Server.View (View(..))

type GroupIdLike = String
type DataIdLike = String

data Action
  = GetIndex
  | GetGroupList
  | GetGroup GroupIdLike
  | GetGroupDataList GroupIdLike
  | CreateGroupData GroupIdLike
  | GetGroupData GroupIdLike DataIdLike
  | UpdateGroupData GroupIdLike DataIdLike

instance showAction :: Show Action where
  show GetIndex = "GetIndex"
  show GetGroupList = "GetGroupList"
  show (GetGroup groupId) = "GetGroup(" <> groupId <> ")"
  show (GetGroupDataList groupId) = "GetGroupDataList(" <> groupId <> ")"
  show (CreateGroupData groupId) = "CreateGroupData(" <> groupId <> ")"
  show (GetGroupData groupId dataId)
    = "GetGroupData(" <> groupId <> "," <> dataId <> ")"
  show (UpdateGroupData groupId dataId)
    = "UpdateGroupData(" <> groupId <> "," <> dataId <> ")"

handleAction :: forall e. Context -> Action -> Request -> Aff (ServerEff e) Response
handleAction context GetGroupList _ = do
  db <- readVar context
  groups <- pure $ findGroupAll db
  view <- pure $ GroupListView groups
  pure $ response200 view
handleAction context (GetGroup groupId) _ = do
  db <- readVar context
  groupMaybe <- pure $ findGroupById db groupId
  case groupMaybe of
    Nothing ->
      pure response404
    Just group -> do
      view <- pure $ GroupView group
      pure $ response200 view
handleAction context (GetGroupDataList groupId) _ = do
  db <- readVar context
  allDataMaybe <- pure $ findDataAllByGroupId db groupId
  case allDataMaybe of
    Nothing ->
      pure response404
    Just allData -> do
      view <- pure $ DataListView allData
      pure $ response200 view
handleAction context (GetGroupData groupId dataId) _ = do
  db <- readVar context
  dataMaybe <- pure $ findDataByGroupIdAndDataId db groupId dataId
  case dataMaybe of
    Nothing ->
      pure response404
    Just d -> do
      view <- pure $ DataView d
      pure $ response200 view
handleAction _ action request = do
  view <- pure $ RequestView request
  pure $ response200 view
