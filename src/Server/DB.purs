module Server.DB
  ( Config
  , Context
  , addData
  , findDataAllByGroupId
  , findDataByGroupIdAndDataId
  , findGroupAll
  , findGroupById
  ) where

import Common.Model (Data, Group, GroupId, DataId)
import Control.Bind (bind, pure, (<$>), (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef)
import Data.Array as Array
import Data.Eq ((==))
import Data.Foldable (find)
import Data.Function (($))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Server.Sheets (addData) as Sheets

type Config =
  { googleApiClientEmail :: String
  , googleApiPrivateKey :: String
  , hostname :: Maybe String
  , port :: Maybe Int
  , spreadsheetId :: String
  }
type Context = Ref { config :: Config,  db :: (Array Group) }

addData
  :: forall e
  . Context
  -> GroupId
  -> Data
  -> Aff (ref :: REF | e) (Maybe Data)
addData context groupId d = do
  { config: config@
    { googleApiClientEmail
    , googleApiPrivateKey
    , spreadsheetId
    }
  , db
  } <- liftEff $ readRef context
  db' <- pure $ addData' db groupId d
  _ <- liftEff $ writeRef context ({ config: config, db: db' })
  case findGroupById' db' groupId of
    Nothing -> pure Nothing
    Just group -> do
      d' <- Sheets.addData
        googleApiClientEmail
        googleApiPrivateKey
        spreadsheetId
        group
        d
      pure $ Just d'

addData' :: Array Group -> GroupId -> Data -> Array Group
addData' db groupId d = fromMaybe db do
  index <- Array.findIndex (\{ id } -> id == groupId) db
  Array.modifyAt
    index
    (\g@{ id: groupId', data: groupData } ->
      case Array.findIndex (\{ id } -> id == d.id) groupData of
        Nothing -> { id: groupId', data: groupData <> [d] }
        Just index' -> do
          case Array.modifyAt index' (_ { value = d.value }) groupData of
            Nothing -> g
            Just newData -> { id: groupId', data: newData }
    )
    db

findDataAllByGroupId
  :: forall e
  . Context
  -> GroupId
  -> Aff (ref :: REF | e) (Maybe (Array Data))
findDataAllByGroupId context groupId = do
  { db } <- liftEff $ readRef context
  pure $ findDataAllByGroupId' db groupId

findDataAllByGroupId' :: Array Group -> GroupId -> Maybe (Array Data)
findDataAllByGroupId' d groupId = _.data <$> (findGroupById' d groupId)

findDataByGroupIdAndDataId
  :: forall e
  . Context
  -> GroupId
  -> DataId
  -> Aff (ref :: REF | e) (Maybe Data)
findDataByGroupIdAndDataId context groupId dataId = do
  { db } <- liftEff $ readRef context
  pure $ findDataByGroupIdAndDataId' db groupId dataId

findDataByGroupIdAndDataId' :: Array Group -> GroupId -> DataId -> Maybe Data
findDataByGroupIdAndDataId' d groupId dataId =
  (findDataAllByGroupId' d groupId) >>= (find (\{ id } -> id == dataId))

findGroupAll
  :: forall e
  . Context
  -> Aff (ref :: REF | e) (Array Group)
findGroupAll context = do
  { db } <- liftEff $ readRef context
  pure $ findGroupAll' db

findGroupAll' :: Array Group -> Array Group
findGroupAll' d = d

findGroupById
  :: forall e
  . Context
  -> GroupId
  -> Aff (ref :: REF | e) (Maybe Group)
findGroupById context groupId = do
  { db } <- liftEff $ readRef context
  pure $ findGroupById' db groupId

findGroupById' :: Array Group -> GroupId -> Maybe Group
findGroupById' d groupId = find (\{ id } -> id == groupId) d
