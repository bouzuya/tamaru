module Server.Main (main) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server.Node (ServerEff, run)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Node.Process (PROCESS, lookupEnv)
import Prelude (Unit, bind, pure, ($), (<$>))
import Server.Action (handleAction)
import Server.Path (parsePath')
import Server.Response (response302, response404)
import Server.Route (route)

type Config =
  { googleApiKey :: String
  , spreadsheetId :: String
  }

onRequest
  :: forall e
  . Request
  -> Aff (ServerEff e) Response
onRequest request@{ method, pathname } = do
  case parsePath' pathname of
    Left location ->
      pure $ response302 location
    Right normalizedPath ->
      case route method normalizedPath of
        Nothing ->
          pure response404
        Just action ->
          handleAction action request

onListen :: forall e. Eff (console :: CONSOLE | e) Unit
onListen = log "listening..."

main
  :: forall e
  . Eff
    (ServerEff
      (console :: CONSOLE, exception :: EXCEPTION, process :: PROCESS | e)
    )
    Unit
main = do
  configMaybe <- runMaybeT do
    googleApiKey <- MaybeT $ lookupEnv "GOOGLE_API_KEY"
    spreadsheetId <- MaybeT $ lookupEnv "SPREADSHEET_ID"
    pure { googleApiKey, spreadsheetId }
  config <- maybe (throw "INVALID ENV") pure configMaybe
  port <- fromMaybe 3000 <$> runMaybeT do
    portString <- MaybeT $ lookupEnv "PORT"
    MaybeT $ pure $ Int.fromString portString
  let serverOptions = { hostname: "0.0.0.0", port }
  run serverOptions onListen onRequest
