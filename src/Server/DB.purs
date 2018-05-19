module Server.DB
  ( Context
  , findDataAllByGroupId
  , findDataByGroupIdAndDataId
  , findGroupAll
  , findGroupById
  ) where

import Control.Bind (bind, pure, (<$>), (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, readVar)
import Control.Monad.Eff.AVar (AVar)
import Data.Eq ((==))
import Data.Foldable (find)
import Data.Function (($))
import Data.Maybe (Maybe)
import Server.Model (Data, Group, GroupId, DataId)

type Context = AVar (Array Group)

findDataAllByGroupId
  :: forall e
  . Context
  -> GroupId
  -> Aff (avar :: AVAR | e) (Maybe (Array Data))
findDataAllByGroupId context groupId = do
  db <- readVar context
  pure $ findDataAllByGroupId' db groupId

findDataAllByGroupId' :: Array Group -> GroupId -> Maybe (Array Data)
findDataAllByGroupId' d groupId = _.data <$> (findGroupById' d groupId)

findDataByGroupIdAndDataId
  :: forall e
  . Context
  -> GroupId
  -> DataId
  -> Aff (avar :: AVAR | e) (Maybe Data)
findDataByGroupIdAndDataId context groupId dataId = do
  db <- readVar context
  pure $ findDataByGroupIdAndDataId' db groupId dataId

findDataByGroupIdAndDataId' :: Array Group -> GroupId -> DataId -> Maybe Data
findDataByGroupIdAndDataId' d groupId dataId =
  (findDataAllByGroupId' d groupId) >>= (find (\{ id } -> id == dataId))

findGroupAll
  :: forall e
  . Context
  -> Aff (avar :: AVAR | e) (Array Group)
findGroupAll context = do
  db <- readVar context
  pure $ findGroupAll' db

findGroupAll' :: Array Group -> Array Group
findGroupAll' d = d

findGroupById
  :: forall e
  . Context
  -> GroupId
  -> Aff (avar :: AVAR | e) (Maybe Group)
findGroupById context groupId = do
  db <- readVar context
  pure $ findGroupById' db groupId

findGroupById' :: Array Group -> GroupId -> Maybe Group
findGroupById' d groupId = find (\{ id } -> id == groupId) d
