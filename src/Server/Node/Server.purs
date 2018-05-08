module Server.Node.Server
  ( Body
  , Header
  , Response
  , StatusCode(..)
  , run
  ) where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Node.Encoding as Encoding
import Node.HTTP (HTTP)
import Node.HTTP as HTTP
import Node.Stream as Stream
import Prelude (Unit, bind, pure, unit, ($))

type Body = String
type Header = Tuple String String
newtype StatusCode = StatusCode Int
type Response = { body :: Body, header :: Header, status :: StatusCode }

setBody
  :: forall e. HTTP.Response -> Body -> Eff (http :: HTTP | e) Unit
setBody response body = do
  let writable = HTTP.responseAsStream response
  _ <- Stream.writeString writable Encoding.UTF8 body $ pure unit
  Stream.end writable $ pure unit

setHeader
  :: forall e. HTTP.Response -> Header -> Eff (http :: HTTP | e) Unit
setHeader response (Tuple name value) =
  HTTP.setHeader response name value

setStatusCode
  :: forall e. HTTP.Response -> StatusCode -> Eff (http :: HTTP | e) Unit
setStatusCode response (StatusCode code) =
  HTTP.setStatusCode response code

writeResponse
  :: forall e. HTTP.Response -> Response -> Eff (http :: HTTP | e) Unit
writeResponse response { body, header, status } = do
  _ <- setStatusCode response status
  _ <- setHeader response header
  setBody response body

handleRequest
  :: forall e
  . Eff (http :: HTTP | e) Response
  -> HTTP.Request
  -> HTTP.Response
  -> Eff (http :: HTTP | e) Unit
handleRequest f request response = do
  myResponse <- f
  writeResponse response myResponse

run
  :: forall e
  . Eff (http :: HTTP | e) Unit
  -> Eff (http :: HTTP | e) Response
  -> Eff (http :: HTTP | e) Unit
run f g = do
  server <- HTTP.createServer (handleRequest g)
  let
    listenOptions =
      { hostname: "0.0.0.0"
      , port: 3000
      , backlog: Nothing
      }
  HTTP.listen server listenOptions f
