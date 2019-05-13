module Server.Sheets
  ( Key
  , SpreadsheetId
  , getGroupList
  , setRows
  ) where

import Common.Model (Data, GroupId, Group)
import Control.Bind (bind, pure, (<$>))
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (catMaybes)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Traversable (sequence)
import Data.Unit (Unit, unit)

type SheetsCredentials =
  { clientEmail :: ClientEmail
  , privateKey :: PrivateKey
  }
type ClientEmail = String
type PrivateKey = String
type Key = String
type SpreadsheetId = String
type Range = String
type Row = Array String

foreign import getRowsImpl
  :: ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Range
  -> Effect (Promise (Array Row))
foreign import getSheetTitlesImpl
  :: ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Effect (Promise (Array String))
foreign import setRowsImpl
  :: ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Range
  -> Array Row
  -> Effect (Promise Unit)

getDataList
  :: SheetsCredentials
  -> SpreadsheetId
  -> GroupId
  -> Aff (Array Data)
getDataList { clientEmail, privateKey } spreadsheetId groupId = do
  let
    range = groupId <> "!A:B"
    eff = getRowsImpl clientEmail privateKey spreadsheetId range
  promise <- liftEffect eff
  rows <- Promise.toAff promise
  pure $ catMaybes (toData <$> rows)
  where
    toData [id, value] = Just { id, value }
    toData _ = Nothing

getGroupIdList
  :: SheetsCredentials
  -> SpreadsheetId
  -> Aff (Array GroupId)
getGroupIdList { clientEmail, privateKey } spreadsheetId = do
  let eff = getSheetTitlesImpl clientEmail privateKey spreadsheetId
  promise <- liftEffect eff
  Promise.toAff promise

getGroupList
  :: SheetsCredentials
  -> SpreadsheetId
  -> Aff (Array Group)
getGroupList credentials@{ clientEmail, privateKey } spreadsheetId = do
  groupIds <- getGroupIdList credentials spreadsheetId
  sequence $ (getGroup credentials spreadsheetId) <$> groupIds
  where
    getGroup c s groupId = do
      dataList <- getDataList c s groupId
      pure { id: groupId, data: dataList }

setRows
  :: SheetsCredentials
  -> SpreadsheetId
  -> Range
  -> Array Row
  -> Aff Unit
setRows { clientEmail, privateKey } spreadsheetId range rows = do
  let eff = setRowsImpl clientEmail privateKey spreadsheetId range rows
  promise <- liftEffect eff
  _ <- Promise.toAff promise
  pure unit
