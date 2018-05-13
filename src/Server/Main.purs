module Server.Main (main) where

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
import Server.HTTP.StatusCode (status200, status302, status404)
import Server.Node.Server (Request, Response, ServerEff, run)
import Server.Route (route)

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

onRequest
  :: forall e
  . Request
  -> Aff (ServerEff e) Response
onRequest { body, headers, method, pathname, searchParams } = do
  case parsePath pathname of
    Left location ->
      pure
        { body: ""
        , headers: [Tuple "Location" location]
        , status: status302
        }
    Right normalizedPath ->
    case route method normalizedPath of
      Nothing ->
        pure
          { body: ""
          , headers: [(Tuple "Content-Type" "text/plain")]
          , status: status404
          }
      Just action ->
        pure
          { body:
              intercalate ", "
                [ "method: " <> show method
                , "pathname: " <> pathname
                , "query: " <> (intercalate "," (show <$> searchParams))
                , "body: " <> body
                , "action: " <> show action
                ]
          , headers: [(Tuple "Content-Type" "text/plain"), (Tuple "X-Foo" "bar")]
          , status: status200
          }

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
