module Server.Config
  ( loadConfig
  ) where

import Prelude

import Effect (Effect)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Node.Process (lookupEnv)
import Server.DB (Config)

loadInt :: String -> Effect (Maybe Int)
loadInt key = map toInt (loadString key)

loadString :: String -> Effect (Maybe String)
loadString key = lookupEnv key

toInt :: Maybe String -> Maybe Int
toInt = maybe Nothing Int.fromString

loadConfig :: Effect (Maybe Config)
loadConfig = runMaybeT do
  basicAuthUserName <- MaybeT $ loadString "TAMARU_BASIC_AUTH_USER_NAME"
  basicAuthPassword <- MaybeT $ loadString "TAMARU_BASIC_AUTH_PASSWORD"
  googleApiClientEmail <- MaybeT $ loadString "TAMARU_GOOGLE_API_CLIENT_EMAIL"
  googleApiPrivateKey' <- MaybeT $ loadString "TAMARU_GOOGLE_API_PRIVATE_KEY"
  spreadsheetId <- MaybeT $ loadString "TAMARU_SPREADSHEET_ID"
  googleApiPrivateKey <- pure $
    String.replaceAll (Pattern "\\n") (Replacement "\n") googleApiPrivateKey'
  hostname <- map Just $ MaybeT $ loadString "HOSTNAME"
  port <- map Just $ MaybeT $ loadInt "PORT"
  pure
    { basicAuthUserName
    , basicAuthPassword
    , googleApiClientEmail
    , googleApiPrivateKey
    , hostname
    , port
    , spreadsheetId
    }
