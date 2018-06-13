module Server.Main (main) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server.Node (ServerEff, run)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF, newRef)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Node.Process (PROCESS, lookupEnv)
import Prelude (Unit, bind, pure, ($), (<$>))
import Server.Action (handleAction)
import Server.DB (Context, Config)
import Server.Path (normalizePath, parsePath')
import Server.Response (response200, response302, response404)
import Server.Route (route)
import Server.Sheets (getGroupList)
import Server.Static (staticRoute, StaticEff)
import Server.View (View(..))

onRequest
  :: forall e
  . Context
  -> Request
  -> Aff
    (ServerEff (StaticEff (ref :: REF | e)))
    Response
onRequest context request@{ method, pathname } = do
  case parsePath' pathname of
    Left location ->
      pure $ response302 location
    Right parsedPath -> do
      match <- liftEff $ staticRoute "public" (normalizePath parsedPath)
      case match of
        Just static ->
          -- TODO: fix mime type
          pure (response200 "application/javascript" (StaticView static))
        Nothing ->
          case route method parsedPath of
            Nothing ->
              pure response404
            Just action ->
              handleAction context action request

onListen :: forall e. Eff (console :: CONSOLE | e) Unit
onListen = log "listening..."

loadConfig :: forall e. Eff (process :: PROCESS | e) (Maybe Config)
loadConfig = runMaybeT do
  googleApiClientEmail <- MaybeT $ lookupEnv "TAMARU_GOOGLE_API_CLIENT_EMAIL"
  googleApiPrivateKey' <- MaybeT $ lookupEnv "TAMARU_GOOGLE_API_PRIVATE_KEY"
  spreadsheetId <- MaybeT $ lookupEnv "TAMARU_SPREADSHEET_ID"
  googleApiPrivateKey <- pure $
    String.replaceAll (Pattern "\\n") (Replacement "\n") googleApiPrivateKey'
  pure { googleApiClientEmail, googleApiPrivateKey, spreadsheetId }

main
  :: forall e
  . Eff
    (ServerEff
      (StaticEff
        ( console :: CONSOLE
        , exception :: EXCEPTION
        , process :: PROCESS
        , ref :: REF
        | e)
      )
    )
    Unit
main = launchAff_ do
  configMaybe <- liftEff loadConfig
  config <- liftEff $ maybe (throw "INVALID ENV") pure configMaybe
  db <-
    getGroupList
      config.googleApiClientEmail
      config.googleApiPrivateKey
      config.spreadsheetId
  context <- liftEff $ newRef { config, db }
  port <- fromMaybe 3000 <$> runMaybeT do
    portString <- MaybeT $ liftEff $ lookupEnv "PORT"
    MaybeT $ liftEff $ pure $ Int.fromString portString
  let serverOptions = { hostname: "0.0.0.0", port }
  liftEff $ run serverOptions onListen (onRequest context)
