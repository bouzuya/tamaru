module Bouzuya.HTTP.Server.Node
  ( Effect
  , run
  ) where

import Bouzuya.HTTP.Header (Header)
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable as Nullable
import Data.StrMap as StrMap
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Global (decodeURIComponent)
import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Node.URL as URL
import Prelude (Unit, bind, map, pure, unit, ($), (<>), (>>>))
import Server.Uint8Array as Uint8Array

type Effect e =
  Uint8Array.Effect
  ( avar :: AVAR
  , buffer :: BUFFER
  , dom :: DOM
  , exception :: EXCEPTION
  , http :: HTTP.HTTP
  , ref :: REF
  | e
  )
type Body = Buffer.Buffer
type ServerOptions =
  { hostname :: String
  , port :: Int
  }

setBody
  :: forall e. HTTP.Response -> Body -> Eff (http :: HTTP.HTTP | e) Unit
setBody response body = do
  let writable = HTTP.responseAsStream response
  _ <- Stream.write writable body $ pure unit
  Stream.end writable $ pure unit

setHeader
  :: forall e. HTTP.Response -> Header -> Eff (http :: HTTP.HTTP | e) Unit
setHeader response (Tuple name value) =
  HTTP.setHeader response name value

setHeaders
  :: forall e. HTTP.Response -> Array Header -> Eff (http :: HTTP.HTTP | e) Unit
setHeaders response headers =
  Foldable.for_ headers (setHeader response)

setStatusCode
  :: forall e. HTTP.Response -> StatusCode -> Eff (http :: HTTP.HTTP | e) Unit
setStatusCode response (StatusCode code message) = do
  _ <- HTTP.setStatusCode response code
  HTTP.setStatusMessage response message

readBody
  :: forall e
  . HTTP.Request
  -> Aff (Effect e) String
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
  b <- AVar.takeVar bv
  liftEff (Buffer.toString Encoding.UTF8 b)

readRequest
  :: forall e
  . HTTP.Request
  -> Aff (Effect e) Request
readRequest request = do
  let
    headers = HTTP.requestHeaders request
    -- TODO: 405 Method Not Allowed ?
    method = fromMaybe Method.GET $
      Method.fromString $ HTTP.requestMethod request
    url = HTTP.requestURL request
    urlObject = URL.parse url
    pathname = fromMaybe "" (Nullable.toMaybe urlObject.pathname)
    searchParams = maybe [] parseQueryString (Nullable.toMaybe urlObject.query)
    parseQueryString :: String -> Array (Tuple String String)
    parseQueryString =
      String.split (Pattern "&")
        >>> map (String.split (Pattern "="))
        >>> map (map decodeURIComponent)
        >>> map
          (
            case _ of
              [k, v] -> Just (Tuple k v)
              _ -> Nothing
          )
        >>> Array.catMaybes
  body <- readBody request
  pure $
    { body
    , headers: StrMap.foldMap (\k v -> [Tuple k v]) headers
    , method
    , pathname
    , searchParams
    }

writeResponse
  :: forall e
  . HTTP.Response
  -> Response
  -> Eff (Uint8Array.Effect (http :: HTTP.HTTP | e)) Unit
writeResponse response { body, headers, status } = do
  _ <- setStatusCode response status
  _ <- setHeaders response headers
  b <- Uint8Array.toBuffer body
  setBody response b

handleRequest
  :: forall e
  . (Request -> Aff (Effect e) Response)
  -> HTTP.Request
  -> HTTP.Response
  -> Eff (Effect e) Unit
handleRequest onRequest request response = Aff.launchAff_ do
  req <- readRequest request
  res <- onRequest req
  liftEff $ writeResponse response res

run
  :: forall e
  . ServerOptions
  -> Eff (Effect e) Unit
  -> (Request -> Aff (Effect e) Response)
  -> Eff (Effect e) Unit
run { hostname, port } onListen onRequest = do
  server <- HTTP.createServer (handleRequest onRequest)
  let
    listenOptions =
      { hostname
      , port
      , backlog: Nothing
      }
  HTTP.listen server listenOptions onListen
