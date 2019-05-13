module Server.Action
  ( Action(..)
  , GroupIdLike
  , DataIdLike
  , handleAction
  ) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server (Effect)
import Bouzuya.Halogen.StringRenderer (render)
import Common.Component.ServerRoot (serverRoot)
import Control.Monad.Aff (Aff)
import Effect.Class (liftEff)
import Effect.Ref (REF)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Either (either, hush)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Prelude (class Show, bind, const, pure, ($), (<>), (=<<))
import Server.DB (Context, addData, findDataAllByGroupId, findDataByGroupIdAndDataId, findGroupAll, findGroupById)
import Server.Response (response200, response400, response404, response500)
import Server.View (View(..))
import Server.View as View

type GroupIdLike = String
type DataIdLike = String

data Action
  = GetIndex
  | GetGroupList
  | GetGroup GroupIdLike
  | GetGroupDataList GroupIdLike
  | UpdateGroupData GroupIdLike
  | GetGroupData GroupIdLike DataIdLike

instance showAction :: Show Action where
  show GetIndex = "GetIndex"
  show GetGroupList = "GetGroupList"
  show (GetGroup groupId) = "GetGroup(" <> groupId <> ")"
  show (GetGroupDataList groupId) = "GetGroupDataList(" <> groupId <> ")"
  show (UpdateGroupData groupId) = "UpdateGroupData(" <> groupId <> ")"
  show (GetGroupData groupId dataId)
    = "GetGroupData(" <> groupId <> "," <> dataId <> ")"

handleAction
  :: forall e
  . Context
  -> Action
  -> Request
  -> Aff Response
handleAction context GetIndex _ = do
  groupList <- findGroupAll context
  view <- pure (IndexView (render serverRoot { groupList }))
  liftEff $ response200 "text/html" view
handleAction context GetGroupList _ = do
  groups <- findGroupAll context
  view <- pure $ GroupListView groups
  liftEff $ response200 "application/json" view
handleAction context (GetGroup groupId) _ = do
  groupMaybe <- findGroupById context groupId
  case groupMaybe of
    Nothing ->
      pure response404
    Just group -> do
      view <- pure $ GroupView group
      liftEff $ response200 "application/json" view
handleAction context (GetGroupDataList groupId) _ = do
  allDataMaybe <- findDataAllByGroupId context groupId
  case allDataMaybe of
    Nothing ->
      pure response404
    Just allData -> do
      view <- pure $ DataListView allData
      liftEff $ response200 "application/json" view
handleAction context (UpdateGroupData groupId) { body } = do
  paramsMaybe <- pure do
    m <- hush (decodeJson =<< jsonParser body)
    id <- Object.lookup "id" m
    value <- Object.lookup "value" m
    pure { id, value }
  case paramsMaybe of
    Nothing ->
      pure response400
    Just params -> do
      groupMaybe <- findGroupById context groupId
      case groupMaybe of
        Nothing ->
          pure response404
        Just group -> do
          updatedMaybe <- addData context group.id params
          case updatedMaybe of
            Nothing ->
              pure response500
            Just d -> do
              view <- pure $ DataView d
              liftEff $ response200 "application/json" view
handleAction context (GetGroupData groupId dataId) _ = do
  dataMaybe <- findDataByGroupIdAndDataId context groupId dataId
  case dataMaybe of
    Nothing ->
      pure response404
    Just d -> do
      view <- pure $ DataView d
      liftEff $ response200 "application/json" view
