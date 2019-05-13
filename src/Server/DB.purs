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
import Control.Monad.Aff (Aff)
import Effect.Class (liftEff)
import Effect.Ref (REF, Ref, readRef, writeRef)
import Data.Array as Array
import Data.Eq ((==))
import Data.Foldable (find)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Prelude (bind, pure, ($), (+), (<$>), (>>=))
import Server.Sheets as Sheets

type Config =
  { basicAuthUserName :: String
  , basicAuthPassword :: String
  , googleApiClientEmail :: String
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
  -> Aff (Maybe Data)
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
      let
        position =
          case Array.findIndex (\{ id: id' } -> d.id == id') group.data of
            Nothing -> (Array.length group.data) + 1
            Just index -> index + 1
        rowNumber = Int.toStringAs Int.decimal position
        range = group.id <> "!A" <> rowNumber <> ":" <> "B" <> rowNumber
        rows = [[d.id, d.value]]
      _ <- Sheets.setRows
        { clientEmail: googleApiClientEmail, privateKey: googleApiPrivateKey }
        spreadsheetId
        range
        rows
      pure $ Just d

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
  -> Aff (Maybe (Array Data))
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
  -> Aff (Maybe Data)
findDataByGroupIdAndDataId context groupId dataId = do
  { db } <- liftEff $ readRef context
  pure $ findDataByGroupIdAndDataId' db groupId dataId

findDataByGroupIdAndDataId' :: Array Group -> GroupId -> DataId -> Maybe Data
findDataByGroupIdAndDataId' d groupId dataId =
  (findDataAllByGroupId' d groupId) >>= (find (\{ id } -> id == dataId))

findGroupAll
  :: forall e
  . Context
  -> Aff (Array Group)
findGroupAll context = do
  { db } <- liftEff $ readRef context
  pure $ findGroupAll' db

findGroupAll' :: Array Group -> Array Group
findGroupAll' d = d

findGroupById
  :: forall e
  . Context
  -> GroupId
  -> Aff (Maybe Group)
findGroupById context groupId = do
  { db } <- liftEff $ readRef context
  pure $ findGroupById' db groupId

findGroupById' :: Array Group -> GroupId -> Maybe Group
findGroupById' d groupId = find (\{ id } -> id == groupId) d
