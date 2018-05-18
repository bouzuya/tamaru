module Server.Sheets
  ( Key
  , SpreadsheetId
  , getGroupList
  ) where

import Control.Bind (bind, pure, (<$>), (>>=))
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
import Server.Model (Data, GroupId, Group)

type Key = String
type SpreadsheetId = String
type Range = String
type Row = Array String

foreign import getRowsImpl
  :: forall e. Key -> SpreadsheetId -> Range -> Eff e (Promise (Array Row))
foreign import getSheetTitlesImpl
  :: forall e. Key -> SpreadsheetId -> Eff e (Promise (Array String))

getDataList :: forall e. Key -> SpreadsheetId -> GroupId -> Aff e (Array Data)
getDataList key spreadsheetId groupId = do
  let range = groupId <> "!A:B"
  rows <- liftEff (getRowsImpl key spreadsheetId range) >>= Promise.toAff
  pure $ catMaybes (toData <$> rows)
  where
    toData [id, value] = Just { id, value }
    toData _ = Nothing

getGroupIdList :: forall e. Key -> SpreadsheetId -> Aff e (Array GroupId)
getGroupIdList key spreadsheetId = do
  liftEff (getSheetTitlesImpl key spreadsheetId) >>= Promise.toAff

getGroupList :: forall e. Key -> SpreadsheetId -> Aff e (Array Group)
getGroupList key spreadsheetId = do
  groupIds <- getGroupIdList key spreadsheetId
  sequence $ (getGroup key spreadsheetId) <$> groupIds
  where
    getGroup k s groupId = do
      dataList <- getDataList k s groupId
      pure { id: groupId, data: dataList }
