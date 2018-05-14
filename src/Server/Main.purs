module Server.Main (main) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server.Node (ServerEff, run)
import Bouzuya.HTTP.StatusCode (status200, status302, status404)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Node.Process (PROCESS, lookupEnv)
import Prelude (Unit, bind, not, pure, show, ($), (<$>), (<<<), (<>), (==))
import Server.DB (db, findGroupAll, findGroupById)
import Server.Route (Action(..), route)
import Server.View (View(..))

joinPath :: Array String -> String
joinPath pathPieces = "/" <> (intercalate "/" pathPieces)

splitPath :: String -> Array String
splitPath path =
  Array.filter (not <<< String.null) $ String.split (Pattern "/") path

parsePath :: String -> Either String (Array String)
parsePath pathname =
  let
    splittedPath = splitPath pathname
    normalizedPath = joinPath splittedPath
  in
    if pathname == normalizedPath
    then Right splittedPath
    else Left normalizedPath

response200 :: View -> Response
response200 view =
  { body: show view
  , headers: [(Tuple "Content-Type" "text/plain")]
  , status: status200
  }

response302 :: String -> Response
response302 location =
  { body: ""
  , headers: [Tuple "Location" location]
  , status: status302
  }

response404 :: Response
response404 =
  { body: ""
  , headers: [(Tuple "Content-Type" "text/plain")]
  , status: status404
  }

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
  case parsePath pathname of
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
