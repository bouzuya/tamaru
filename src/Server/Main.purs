module Server.Main (main) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server as Server
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF, newRef)
import Data.Either (Either(..))
import Data.Foldable (elem, find)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Prelude (Unit, bind, map, otherwise, pure, show, ($), (&&), (<>), (==))
import Server.Action (handleAction)
import Server.Config as Config
import Server.DB (Context)
import Server.Path (normalizePath, parsePath')
import Server.Response (response200, response302, response401, response404)
import Server.Route (route)
import Server.Sheets (getGroupList)
import Server.Static as Static
import Server.View (View(..))

type Effect e =
  (Server.Effect
    (Static.Effect
      (Config.Effect
        ( console :: CONSOLE
        , exception :: EXCEPTION
        , ref :: REF
        | e
        )
      )
    )
  )

type Extension = String -- ".html"
type ExtensionWithoutPeriod = String -- "html"
type MimeType = String -- "text/html"

type MimeTypeRecord = Tuple MimeType (Array ExtensionWithoutPeriod)

base64encode :: forall e. String -> Eff (buffer :: BUFFER | e) String
base64encode s = do
  b <- Buffer.fromString s Encoding.UTF8
  Buffer.toString Encoding.Base64 b

lookupMimeType :: Extension -> Array MimeTypeRecord -> Maybe MimeType
lookupMimeType extension records = do
  e <- getExtensionWithoutPeriod extension
  found <- find (match e) records
  pure (fst found)
  where
  getExtensionWithoutPeriod :: Extension -> Maybe ExtensionWithoutPeriod
  getExtensionWithoutPeriod e = map _.after (String.splitAt 1 e)
  match :: ExtensionWithoutPeriod -> MimeTypeRecord -> Boolean
  match e record = elem e (snd record)

mimeTypeRecords :: Array MimeTypeRecord
mimeTypeRecords =
  [ Tuple "application/javascript" ["js"]
  , Tuple "image/jpeg" ["jpeg", "jpg"]
  , Tuple "image/png" ["png"]
  , Tuple "text/css" ["css"]
  , Tuple "text/html" ["html"]
  , Tuple "text/plain" ["txt"]
  ]

isAuthenticated :: String -> Request -> Boolean
isAuthenticated auth { headers } =
  isJust
    (find
      (\(Tuple name value) ->
        name == "authorization"
        && value == "Basic " <> auth)
      headers)

onRequest
  :: forall e
  . String
  -> Context
  -> Request
  -> Aff
    (Server.Effect (Static.Effect (console :: CONSOLE, ref :: REF | e)))
    Response
onRequest auth context request@{ headers, method, pathname }
  | isAuthenticated auth request = do
    case parsePath' pathname of
      Left location ->
        pure $ response302 location
      Right parsedPath -> do
        match <- liftEff $ Static.staticRoute "public" (normalizePath parsedPath)
        case match of
          Just { binary, extension } ->
            let
              defaultMimeType = "application/octet-stream"
              mimeType =
                fromMaybe
                  defaultMimeType
                  (lookupMimeType extension mimeTypeRecords)
            in
              liftEff $ response200 mimeType (StaticView binary)
          Nothing ->
            case route method parsedPath of
              Nothing ->
                pure response404
              Just action ->
                handleAction context action request
  | otherwise = pure response401

onListen
  :: forall e
  . { hostname :: String, port :: Int }
  -> Eff (console :: CONSOLE | e) Unit
onListen { hostname, port } = do
  _ <- log "listening..."
  _ <- log $ "http://" <> hostname <> ":" <> show port
  log ""

main :: forall e. Eff (Effect e) Unit
main = launchAff_ do
  configMaybe <- liftEff Config.loadConfig
  config <- liftEff $ maybe (throw "INVALID ENV") pure configMaybe
  db <-
    getGroupList
      { clientEmail: config.googleApiClientEmail
      , privateKey: config.googleApiPrivateKey
      }
      config.spreadsheetId
  context <- liftEff $ newRef { config, db }
  let { basicAuthUserName, basicAuthPassword } = config
  auth <- liftEff $ base64encode $ basicAuthUserName <> ":" <> basicAuthPassword
  port <- pure $ fromMaybe 3000 config.port
  hostname <- pure $ fromMaybe "0.0.0.0" config.hostname
  let serverOptions = { hostname, port }
  liftEff $
    Server.run
      serverOptions
      (onListen { hostname, port })
      (onRequest auth context)
