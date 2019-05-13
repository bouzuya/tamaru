module Server.DB
  ( Config
  , Context
  , addData
  , findDataAllByGroupId
  , findDataByGroupIdAndDataId
  , findGroupAll
  , findGroupById
  ) where

import Prelude

import Common.Model (Data, Group, GroupId, DataId)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.Array as Array
import Data.Foldable (find)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
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
  :: Context
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
  } <- liftEffect $ Ref.read context
  db' <- pure $ addData' db groupId d
  _ <- liftEffect $ Ref.write ({ config: config, db: db' }) context
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
  :: Context
  -> GroupId
  -> Aff (Maybe (Array Data))
findDataAllByGroupId context groupId = do
  { db } <- liftEffect $ Ref.read context
  pure $ findDataAllByGroupId' db groupId

findDataAllByGroupId' :: Array Group -> GroupId -> Maybe (Array Data)
findDataAllByGroupId' d groupId = _.data <$> (findGroupById' d groupId)

findDataByGroupIdAndDataId
  :: Context
  -> GroupId
  -> DataId
  -> Aff (Maybe Data)
findDataByGroupIdAndDataId context groupId dataId = do
  { db } <- liftEffect $ Ref.read context
  pure $ findDataByGroupIdAndDataId' db groupId dataId

findDataByGroupIdAndDataId' :: Array Group -> GroupId -> DataId -> Maybe Data
findDataByGroupIdAndDataId' d groupId dataId =
  (findDataAllByGroupId' d groupId) >>= (find (\{ id } -> id == dataId))

findGroupAll
  :: Context
  -> Aff (Array Group)
findGroupAll context = do
  { db } <- liftEffect $ Ref.read context
  pure $ findGroupAll' db

findGroupAll' :: Array Group -> Array Group
findGroupAll' d = d

findGroupById
  :: Context
  -> GroupId
  -> Aff (Maybe Group)
findGroupById context groupId = do
  { db } <- liftEffect $ Ref.read context
  pure $ findGroupById' db groupId

findGroupById' :: Array Group -> GroupId -> Maybe Group
findGroupById' d groupId = find (\{ id } -> id == groupId) d
