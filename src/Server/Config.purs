module Server.Config
  ( Effect
  , loadConfig
  ) where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Node.Process (PROCESS, lookupEnv)
import Prelude (bind, map, pure, ($))
import Server.DB (Config)

type Effect e = (process :: PROCESS | e)

loadInt :: forall e. String -> Eff (Effect e) (Maybe Int)
loadInt key = map toInt (loadString key)

loadString :: forall e. String -> Eff (Effect e) (Maybe String)
loadString key = lookupEnv key

toInt :: Maybe String -> Maybe Int
toInt = maybe Nothing Int.fromString

loadConfig :: forall e. Eff (Effect e) (Maybe Config)
loadConfig = runMaybeT do
  googleApiClientEmail <- MaybeT $ loadString "TAMARU_GOOGLE_API_CLIENT_EMAIL"
  googleApiPrivateKey' <- MaybeT $ loadString "TAMARU_GOOGLE_API_PRIVATE_KEY"
  spreadsheetId <- MaybeT $ loadString "TAMARU_SPREADSHEET_ID"
  googleApiPrivateKey <- pure $
    String.replaceAll (Pattern "\\n") (Replacement "\n") googleApiPrivateKey'
  hostname <- map Just $ MaybeT $ loadString "HOSTNAME"
  port <- map Just $ MaybeT $ loadInt "PORT"
  pure { googleApiClientEmail, googleApiPrivateKey, hostname, port, spreadsheetId }
