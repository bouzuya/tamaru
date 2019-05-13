module Bouzuya.HTTP.Server.Node
  ( run
  ) where

import Prelude

import Bouzuya.HTTP.Header (Header)
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode (StatusCode(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable as Nullable
import Foreign.Object as Object
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeDecodeURIComponent)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Node.URL as URL
import Server.Uint8Array as Uint8Array

type Body = Buffer.Buffer
type ServerOptions =
  { hostname :: String
  , port :: Int
  }

setBody :: HTTP.Response -> Body -> Effect Unit
setBody response body = do
  let writable = HTTP.responseAsStream response
  _ <- Stream.write writable body $ pure unit
  Stream.end writable $ pure unit

setHeader :: HTTP.Response -> Header -> Effect Unit
setHeader response (Tuple name value) =
  HTTP.setHeader response name value

setHeaders :: HTTP.Response -> Array Header -> Effect Unit
setHeaders response headers =
  Foldable.for_ headers (setHeader response)

setStatusCode :: HTTP.Response -> StatusCode -> Effect Unit
setStatusCode response (StatusCode code message) = do
  _ <- HTTP.setStatusCode response code
  HTTP.setStatusMessage response message

readBody :: HTTP.Request -> Aff String
readBody request = do
  let readable = HTTP.requestAsStream request
  bv <- AVar.empty
  bsv <- AVar.new []
  -- TODO: check exception
  _ <- liftEffect $ Stream.onData readable \b -> Aff.launchAff_ do
    bs <- AVar.take bsv
    AVar.put (bs <> [b]) bsv
  _ <- liftEffect $ Stream.onError readable \e -> Aff.launchAff_ do
    AVar.kill e bv
  _ <- liftEffect $ Stream.onEnd readable $ Aff.launchAff_ do
    bs <- AVar.take bsv
    b <- liftEffect (Buffer.concat bs)
    AVar.put b bv
  b <- AVar.take bv
  liftEffect (Buffer.toString Encoding.UTF8 b)

readRequest :: HTTP.Request -> Aff Request
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
        >>> map (map unsafeDecodeURIComponent)
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
    , headers: Object.foldMap (\k v -> [Tuple k v]) headers
    , method
    , pathname
    , searchParams
    }

writeResponse :: HTTP.Response -> Response -> Effect Unit
writeResponse response { body, headers, status } = do
  _ <- setStatusCode response status
  _ <- setHeaders response headers
  b <- Uint8Array.toBuffer body
  setBody response b

handleRequest :: (Request -> Aff Response)
  -> HTTP.Request
  -> HTTP.Response
  -> Effect Unit
handleRequest onRequest request response = Aff.launchAff_ do
  req <- readRequest request
  res <- onRequest req
  liftEffect $ writeResponse response res

run
  :: ServerOptions
  -> Effect Unit
  -> (Request -> Aff Response)
  -> Effect Unit
run { hostname, port } onListen onRequest = do
  server <- HTTP.createServer (handleRequest onRequest)
  let
    listenOptions =
      { hostname
      , port
      , backlog: Nothing
      }
  HTTP.listen server listenOptions onListen
