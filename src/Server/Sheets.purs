module Server.Sheets
  ( Key
  , SpreadsheetId
  , getGroupList
  , setRows
  ) where

import Common.Model (Data, GroupId, Group)
import Control.Bind (bind, pure, (<$>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
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
  :: forall e
  . ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Range
  -> Eff e (Promise (Array Row))
foreign import getSheetTitlesImpl
  :: forall e
  . ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Eff e (Promise (Array String))
foreign import setRowsImpl
  :: forall e
  . ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Range
  -> Array Row
  -> Eff e (Promise Unit)

getDataList
  :: forall e
  . SheetsCredentials
  -> SpreadsheetId
  -> GroupId
  -> Aff e (Array Data)
getDataList { clientEmail, privateKey } spreadsheetId groupId = do
  let
    range = groupId <> "!A:B"
    eff = getRowsImpl clientEmail privateKey spreadsheetId range
  promise <- liftEff eff
  rows <- Promise.toAff promise
  pure $ catMaybes (toData <$> rows)
  where
    toData [id, value] = Just { id, value }
    toData _ = Nothing

getGroupIdList
  :: forall e
  . SheetsCredentials
  -> SpreadsheetId
  -> Aff e (Array GroupId)
getGroupIdList { clientEmail, privateKey } spreadsheetId = do
  let eff = getSheetTitlesImpl clientEmail privateKey spreadsheetId
  promise <- liftEff eff
  Promise.toAff promise

getGroupList
  :: forall e
  . SheetsCredentials
  -> SpreadsheetId
  -> Aff e (Array Group)
getGroupList credentials@{ clientEmail, privateKey } spreadsheetId = do
  groupIds <- getGroupIdList credentials spreadsheetId
  sequence $ (getGroup credentials spreadsheetId) <$> groupIds
  where
    getGroup c s groupId = do
      dataList <- getDataList c s groupId
      pure { id: groupId, data: dataList }

setRows
  :: forall e
  . SheetsCredentials
  -> SpreadsheetId
  -> Range
  -> Array Row
  -> Aff e Unit
setRows { clientEmail, privateKey } spreadsheetId range rows = do
  let eff = setRowsImpl clientEmail privateKey spreadsheetId range rows
  promise <- liftEff eff
  _ <- Promise.toAff promise
  pure unit
