module Server.Sheets
  ( Key
  , SpreadsheetId
  , addData
  , getGroupList
  ) where

import Control.Bind (bind, pure, (<$>), (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Eq ((==))
import Data.Function (($))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Traversable (sequence)
import Data.Unit (Unit)
import Server.Model (Data, GroupId, Group)

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
  . ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> GroupId
  -> Aff e (Array Data)
getDataList clientEmail privateKey spreadsheetId groupId = do
  let
    range = groupId <> "!A:B"
    eff = getRowsImpl clientEmail privateKey spreadsheetId range
  rows <- liftEff eff >>= Promise.toAff
  pure $ catMaybes (toData <$> rows)
  where
    toData [id, value] = Just { id, value }
    toData _ = Nothing

getGroupIdList
  :: forall e
  . ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Aff e (Array GroupId)
getGroupIdList clientEmail privateKey spreadsheetId = do
  let eff = getSheetTitlesImpl clientEmail privateKey spreadsheetId
  liftEff eff >>= Promise.toAff

getGroupList
  :: forall e
  . ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Aff e (Array Group)
getGroupList clientEmail privateKey spreadsheetId = do
  groupIds <- getGroupIdList clientEmail privateKey spreadsheetId
  sequence $ (getGroup clientEmail privateKey spreadsheetId) <$> groupIds
  where
    getGroup e k s groupId = do
      dataList <- getDataList e k s groupId
      pure { id: groupId, data: dataList }

addData
  :: forall e
  . ClientEmail
  -> PrivateKey
  -> SpreadsheetId
  -> Group
  -> Data
  -> Aff e Data
addData clientEmail privateKey spreadsheetId group { id, value } = do
  let
    position =
      case Array.findIndex (\{ id: id' } -> id == id') group.data of
        Nothing -> Array.length group.data
        Just index -> index + 1
    rowNumber = Int.toStringAs Int.decimal position
    range = "A" <> rowNumber <> ":" <> "B" <> rowNumber
    rows = [[id, value]]
    eff = setRowsImpl clientEmail privateKey spreadsheetId range rows
  _ <- liftEff eff >>= Promise.toAff
  pure { id, value }
