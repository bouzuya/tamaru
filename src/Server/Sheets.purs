module Server.Sheets
  ( Key
  , Range
  , SpreadsheetId
  , getDataList
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Server.Model (Data)

type Key = String
type SpreadsheetId = String
type Range = String
type Row = Array String
data Client

foreign import createClientImpl :: forall e. Key -> Eff e Client
foreign import getRowsImpl
  :: forall e. Client -> SpreadsheetId -> Range -> Eff e (Promise (Array Row))

getDataList :: forall e. Aff e (Array Data)
getDataList = do
  let
    key = "__KEY__"
    spreadsheetId = "__SPREADSHEET_ID__"
    range = "A:B"
  client <- liftEff $ createClientImpl key
  rows <- liftEff (getRowsImpl client spreadsheetId range) >>= Promise.toAff
  pure $ catMaybes (map toData rows)
  where
    toData [id, value] = Just { id, value }
    toData _ = Nothing
