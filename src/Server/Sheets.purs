module Server.Sheets
  ( Key
  , Range
  , SpreadsheetId
  , getValues
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Foldable (intercalate)

type Key = String
type SpreadsheetId = String
type Range = String
data Client

foreign import createClientImpl :: forall e. Key -> Eff e Client
foreign import getValuesImpl
  :: forall e. Client -> SpreadsheetId -> Range -> Eff e (Promise Values)

type Values = Array Row
type Row = Array String

getValues' :: forall e. Client -> SpreadsheetId -> Range -> Aff e Values
getValues' client id range =
  liftEff (getValuesImpl client id range) >>= Promise.toAff

getValues :: forall e. Aff (console :: CONSOLE | e) Unit
getValues = do
  let
    key = "__KEY__"
    spreadsheetId = "__SPREADSHEET_ID__"
    range = "__RANGE__"
  client <- liftEff $ createClientImpl key
  values <- getValues' client spreadsheetId range
  log $ intercalate "\n" ((intercalate ",") <$> values)
