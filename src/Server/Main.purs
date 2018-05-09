module Server.Main (main) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (intercalate)
import Data.Tuple (Tuple(..))
import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Prelude (Unit, bind, pure, ($), (<>))
import Server.Node.Server (HTTP, Request, Response, StatusCode(..), run)

onRequest
  :: forall e
  . Request
  -> Aff (avar :: AVAR, buffer :: BUFFER, http :: HTTP | e) Response
onRequest { body, headers, method, url } = do
  bodyAsString <- liftEff $ Buffer.toString Encoding.UTF8 body
  pure
    { body:
        intercalate ", "
          [ "method: " <> method
          , "url: " <> url
          , "body: " <> bodyAsString
          ]
    , headers: [(Tuple "Content-Type" "text/plain"), (Tuple "X-Foo" "bar")]
    , status: (StatusCode 200)
    }

onListen :: forall e. Eff (console :: CONSOLE | e) Unit
onListen = log "listening..."

main
  :: forall e
  . Eff
    (avar :: AVAR, buffer :: BUFFER, console :: CONSOLE, http :: HTTP | e)
    Unit
main = do
  let options = { hostname: "0.0.0.0", port: 3000 }
  run options onListen onRequest
