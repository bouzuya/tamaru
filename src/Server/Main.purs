module Server.Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(..))
import Prelude (Unit, pure, (<>))
import Server.Node.Server (HTTP, Request, Response, StatusCode(..), run)

onRequest :: forall e. Request -> Eff (http :: HTTP | e) Response
onRequest { headers, method, url } = do
  pure
    { body: "method: " <> method <> ", url: " <> url
    , headers: [(Tuple "Content-Type" "text/plain"), (Tuple "X-Foo" "bar")]
    , status: (StatusCode 200)
    }

onListen :: forall e. Eff (console :: CONSOLE | e) Unit
onListen = log "listening..."

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = do
  let options = { hostname: "0.0.0.0", port: 3000 }
  run options onListen onRequest
