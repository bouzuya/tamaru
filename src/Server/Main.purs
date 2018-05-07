module Server.Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Node.Encoding as Encoding
import Node.HTTP (HTTP)
import Node.HTTP as HTTP
import Node.Stream as Stream
import Prelude (Unit, bind, pure, unit, ($))

handleRequest
  :: forall e
  . HTTP.Request
  -> HTTP.Response
  -> Eff (console :: CONSOLE, http :: HTTP | e) Unit
handleRequest _request response = do
  _ <- HTTP.setStatusCode response 200
  _ <- HTTP.setHeader response "Content-Type" "text/plain"
  let
    writable = HTTP.responseAsStream response
  _ <- Stream.writeString writable Encoding.UTF8 "OK" $ pure unit
  Stream.end writable $ pure unit

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = do
  server <- HTTP.createServer handleRequest
  let
    listenOptions =
      { hostname: "0.0.0.0"
      , port: 3000
      , backlog: Nothing
      }
  HTTP.listen server listenOptions (log "listening...")
