module Server.Main (main) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (intercalate)
import Data.Tuple (Tuple(..))
import Prelude (Unit, pure, (<>))
import Server.Node.Server (Request, Response, ServerEff, StatusCode(..), run)

onRequest
  :: forall e
  . Request
  -> Aff (ServerEff e) Response
onRequest { body, headers, method, url } = do
  pure
    { body:
        intercalate ", "
          [ "method: " <> method
          , "url: " <> url
          , "body: " <> body
          ]
    , headers: [(Tuple "Content-Type" "text/plain"), (Tuple "X-Foo" "bar")]
    , status: (StatusCode 200)
    }

onListen :: forall e. Eff (console :: CONSOLE | e) Unit
onListen = log "listening..."

main
  :: forall e
  . Eff
    (ServerEff (console :: CONSOLE | e))
    Unit
main = do
  let options = { hostname: "0.0.0.0", port: 3000 }
  run options onListen onRequest
