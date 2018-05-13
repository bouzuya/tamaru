module Server.Main (main) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Node.Process (PROCESS, lookupEnv)
import Prelude (Unit, bind, not, pure, show, ($), (<$>), (<<<), (<>))
import Server.HTTP.StatusCode (status200, status404)
import Server.Node.Server (Request, Response, ServerEff, run)
import Server.Route (route)

onRequest
  :: forall e
  . Request
  -> Aff (ServerEff e) Response
onRequest { body, headers, method, pathname, searchParams } = do
  let
    pathSegments = String.split (Pattern "/") pathname
    normalizedPath = Array.filter (not <<< String.null) pathSegments
  case route method normalizedPath of
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
    Nothing ->
      pure
        { body: ""
        , headers: [(Tuple "Content-Type" "text/plain")]
        , status: status404
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
