module Server.Config
  (loadConfig) where

import Control.Monad.Eff (Eff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Node.Process (PROCESS, lookupEnv)
import Prelude (bind, pure, ($))
import Server.DB (Config)

loadConfig :: forall e. Eff (process :: PROCESS | e) (Maybe Config)
loadConfig = runMaybeT do
  googleApiClientEmail <- MaybeT $ lookupEnv "TAMARU_GOOGLE_API_CLIENT_EMAIL"
  googleApiPrivateKey' <- MaybeT $ lookupEnv "TAMARU_GOOGLE_API_PRIVATE_KEY"
  spreadsheetId <- MaybeT $ lookupEnv "TAMARU_SPREADSHEET_ID"
  googleApiPrivateKey <- pure $
    String.replaceAll (Pattern "\\n") (Replacement "\n") googleApiPrivateKey'
  pure { googleApiClientEmail, googleApiPrivateKey, spreadsheetId }
