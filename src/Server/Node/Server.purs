module Server.Node.Server
  ( Body
  , Header
  , Request
  , Response
  , ServerEff
  , StatusCode(..)
  , run
  ) where

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Foldable as Array
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Prelude (Unit, bind, pure, unit, ($), (<>))

type ServerEff e = (avar :: AVAR, buffer :: BUFFER, http :: HTTP.HTTP | e)
type Body = String
type Header = Tuple String String
newtype StatusCode = StatusCode Int
type Request =
  { body :: Buffer
  , headers :: Array Header
  , method :: String
  , url :: String }
type Response =
  { body :: Body
  , headers :: Array Header
  , status :: StatusCode
  }
type ServerOptions =
  { hostname :: String
  , port :: Int
  }

setBody
  :: forall e. HTTP.Response -> Body -> Eff (http :: HTTP.HTTP | e) Unit
setBody response body = do
  let writable = HTTP.responseAsStream response
  _ <- Stream.writeString writable Encoding.UTF8 body $ pure unit
  Stream.end writable $ pure unit

setHeader
  :: forall e. HTTP.Response -> Header -> Eff (http :: HTTP.HTTP | e) Unit
setHeader response (Tuple name value) =
  HTTP.setHeader response name value

setHeaders
  :: forall e. HTTP.Response -> Array Header -> Eff (http :: HTTP.HTTP | e) Unit
setHeaders response headers =
  Array.for_ headers (setHeader response)

setStatusCode
  :: forall e. HTTP.Response -> StatusCode -> Eff (http :: HTTP.HTTP | e) Unit
setStatusCode response (StatusCode code) =
  HTTP.setStatusCode response code

readBody
  :: forall e
  . HTTP.Request
  -> Aff (ServerEff e) Buffer
readBody request = do
  let readable = HTTP.requestAsStream request
  bv <- AVar.makeEmptyVar
  bsv <- AVar.makeVar []
  -- TODO: check exception
  _ <- liftEff' $ Stream.onData readable \b -> Aff.launchAff_ do
    bs <- AVar.takeVar bsv
    AVar.putVar (bs <> [b]) bsv
  _ <- liftEff' $ Stream.onError readable \e -> Aff.launchAff_ do
    AVar.killVar e bv
  _ <- liftEff' $ Stream.onEnd readable $ Aff.launchAff_ do
    bs <- AVar.takeVar bsv
    b <- liftEff (Buffer.concat bs)
    AVar.putVar b bv
  AVar.takeVar bv

readRequest
  :: forall e
  . HTTP.Request
  -> Aff (ServerEff e) Request
readRequest request = do
  let
    headers = HTTP.requestHeaders request
    method = HTTP.requestMethod request
    url = HTTP.requestURL request
  body <- readBody request
  pure $
    { body
    , headers: StrMap.foldMap (\k v -> [Tuple k v]) headers
    , method
    , url
    }

writeResponse
  :: forall e. HTTP.Response -> Response -> Eff (http :: HTTP.HTTP | e) Unit
writeResponse response { body, headers, status } = do
  _ <- setStatusCode response status
  _ <- setHeaders response headers
  setBody response body

handleRequest
  :: forall e
  . (Request -> Aff (ServerEff e) Response)
  -> HTTP.Request
  -> HTTP.Response
  -> Eff (ServerEff e) Unit
handleRequest onRequest request response = Aff.launchAff_ do
  req <- readRequest request
  res <- onRequest req
  liftEff $ writeResponse response res

run
  :: forall e
  . ServerOptions
  -> Eff (ServerEff e) Unit
  -> (Request -> Aff (ServerEff e) Response)
  -> Eff (ServerEff e) Unit
run { hostname, port } onListen onRequest = do
  server <- HTTP.createServer (handleRequest onRequest)
  let
    listenOptions =
      { hostname
      , port
      , backlog: Nothing
      }
  HTTP.listen server listenOptions onListen
