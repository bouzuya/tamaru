module Server.Node.Server
  ( Body
  , HTTP
  , Header
  , Request
  , Response
  , StatusCode(..)
  , run
  ) where

import Control.Monad.Eff (Eff)
import Data.Foldable as Array
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Prelude (Unit, bind, pure, unit, ($))

type HTTP = HTTP.HTTP
type Body = String
type Header = Tuple String String
newtype StatusCode = StatusCode Int
type Request =
  { headers :: Array Header
  , method :: String
  , url :: String }
type Response =
  { body :: Body
  , headers :: Array Header
  , status :: StatusCode
  }

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

setHeaders
  :: forall e. HTTP.Response -> Array Header -> Eff (http :: HTTP | e) Unit
setHeaders response headers =
  Array.for_ headers (setHeader response)

setStatusCode
  :: forall e. HTTP.Response -> StatusCode -> Eff (http :: HTTP | e) Unit
setStatusCode response (StatusCode code) =
  HTTP.setStatusCode response code

readRequest
  :: forall e. HTTP.Request -> Eff (http :: HTTP | e) Request
readRequest request = do
  let
    url = HTTP.requestURL request
    method = HTTP.requestMethod request
    headers = HTTP.requestHeaders request
  pure $
    { headers: StrMap.foldMap (\k v -> [Tuple k v]) headers
    , method
    , url
    }

writeResponse
  :: forall e. HTTP.Response -> Response -> Eff (http :: HTTP | e) Unit
writeResponse response { body, headers, status } = do
  _ <- setStatusCode response status
  _ <- setHeaders response headers
  setBody response body

handleRequest
  :: forall e
  . (Request -> Eff (http :: HTTP | e) Response)
  -> HTTP.Request
  -> HTTP.Response
  -> Eff (http :: HTTP | e) Unit
handleRequest f request response = do
  req <- readRequest request
  res <- f req
  writeResponse response res

run
  :: forall e
  . Eff (http :: HTTP.HTTP | e) Unit
  -> (Request -> Eff (http :: HTTP.HTTP | e) Response)
  -> Eff (http :: HTTP.HTTP | e) Unit
run f g = do
  server <- HTTP.createServer (handleRequest g)
  let
    listenOptions =
      { hostname: "0.0.0.0"
      , port: 3000
      , backlog: Nothing
      }
  HTTP.listen server listenOptions f
