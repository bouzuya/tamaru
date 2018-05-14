module Server.Main (main) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server.Node (ServerEff, run)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Node.Process (PROCESS, lookupEnv)
import Prelude (Unit, bind, pure, ($), (<$>))
import Server.DB (db, findGroupAll, findGroupById)
import Server.Path (parsePath')
import Server.Response (response200, response302, response404)
import Server.Route (Action(..), route)
import Server.View (View(..))

handleAction :: forall e. Action -> Request -> Aff (ServerEff e) Response
handleAction GetGroupList _ = do
  groups <- pure $ findGroupAll db
  view <- pure $ GroupListView groups
  pure $ response200 view
handleAction (GetGroup groupId) _ = do
  groupMaybe <- pure $ findGroupById db groupId
  case groupMaybe of
    Nothing ->
      pure response404
    Just group -> do
      view <- pure $ GroupView group
      pure $ response200 view
handleAction action request = do
  view <- pure $ RequestView request
  pure $ response200 view

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
    (ServerEff (console :: CONSOLE, process :: PROCESS | e))
    Unit
main = do
  port <- fromMaybe 3000 <$> runMaybeT do
    portString <- MaybeT $ lookupEnv "PORT"
    MaybeT $ pure $ Int.fromString portString
  let options = { hostname: "0.0.0.0", port }
  run options onListen onRequest
