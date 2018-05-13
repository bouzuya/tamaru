module Server.Main (main) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Node.Process (PROCESS, lookupEnv)
import Prelude (Unit, bind, pure, show, ($), (<$>), (<>))
import Server.HTTP.StatusCode (status200)
import Server.Node.Server (Request, Response, ServerEff, run)

onRequest
  :: forall e
  . Request
  -> Aff (ServerEff e) Response
onRequest { body, headers, method, pathname, searchParams } = do
  pure
    { body:
        intercalate ", "
          [ "method: " <> show method
          , "pathname: " <> pathname
          , "query: " <> (intercalate "," (show <$> searchParams))
          , "body: " <> body
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
