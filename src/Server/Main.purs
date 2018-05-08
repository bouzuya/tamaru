module Server.Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(..))
import Prelude (Unit, pure, (<>))
import Server.Node.Server (HTTP, Request, Response, StatusCode(..), run)

handleRequest
  :: forall e. Request -> Eff (http :: HTTP | e) Response
handleRequest { headers, method, url } = do
  pure
    { body: "method: " <> method <> ", url: " <> url
    , headers: [(Tuple "Content-Type" "text/plain"), (Tuple "X-Foo" "bar")]
    , status: (StatusCode 200)
    }

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = do
  run { hostname: "0.0.0.0", port: 3000 } (log "listening...") handleRequest
