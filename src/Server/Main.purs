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
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Prelude (Unit, bind, pure, ($))
import Server.Action (handleAction)
import Server.Config as Config
import Server.DB (Context)
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

main
  :: forall e
  . Eff
    (ServerEff
      (StaticEff
        (Config.Effect
          ( console :: CONSOLE
          , exception :: EXCEPTION
          , ref :: REF
          | e
          )
        )
      )
    )
    Unit
main = launchAff_ do
  configMaybe <- liftEff Config.loadConfig
  config <- liftEff $ maybe (throw "INVALID ENV") pure configMaybe
  db <-
    getGroupList
      config.googleApiClientEmail
      config.googleApiPrivateKey
      config.spreadsheetId
  context <- liftEff $ newRef { config, db }
  port <- pure $ fromMaybe 3000 config.port
  let serverOptions = { hostname: "0.0.0.0", port }
  liftEff $ run serverOptions onListen (onRequest context)
