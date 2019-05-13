module Server.Main (main) where

import Prelude

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server as Server
import Effect.Aff (Aff, launchAff_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Console as Console
import Effect.Exception as Exception
import Data.Either (Either(..))
import Data.Foldable (elem, find)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..), fst, snd)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Server.Action (handleAction)
import Server.Config as Config
import Server.DB (Context)
import Server.Path (normalizePath, parsePath')
import Server.Response (response200, response302, response401, response404)
import Server.Route (route)
import Server.Sheets (getGroupList)
import Server.Static as Static
import Server.View (View(..))

type Extension = String -- ".html"
type ExtensionWithoutPeriod = String -- "html"
type MimeType = String -- "text/html"

type MimeTypeRecord = Tuple MimeType (Array ExtensionWithoutPeriod)

base64encode :: String -> Effect String
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
  getExtensionWithoutPeriod e = pure (CodeUnits.splitAt 1 e).after
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

onRequest :: String -> Context -> Request -> Aff Response
onRequest auth context request@{ headers, method, pathname }
  | isAuthenticated auth request = do
    case parsePath' pathname of
      Left location ->
        pure $ response302 location
      Right parsedPath -> do
        match <- liftEffect $ Static.staticRoute "public" (normalizePath parsedPath)
        case match of
          Just { binary, extension } ->
            let
              defaultMimeType = "application/octet-stream"
              mimeType =
                fromMaybe
                  defaultMimeType
                  (lookupMimeType extension mimeTypeRecords)
            in
              liftEffect $ response200 mimeType (StaticView binary)
          Nothing ->
            case route method parsedPath of
              Nothing ->
                pure response404
              Just action ->
                handleAction context action request
  | otherwise = pure response401

onListen :: { hostname :: String, port :: Int } -> Effect Unit
onListen { hostname, port } = do
  _ <- Console.log "listening..."
  _ <- Console.log $ "http://" <> hostname <> ":" <> show port
  _ <- Console.log ""
  pure unit

main :: Effect Unit
main = launchAff_ do
  configMaybe <- liftEffect Config.loadConfig
  config <- liftEffect $ maybe (Exception.throw "INVALID ENV") pure configMaybe
  db <-
    getGroupList
      { clientEmail: config.googleApiClientEmail
      , privateKey: config.googleApiPrivateKey
      }
      config.spreadsheetId
  context <- liftEffect $ Ref.new { config, db }
  let { basicAuthUserName, basicAuthPassword } = config
  auth <- liftEffect $ base64encode $ basicAuthUserName <> ":" <> basicAuthPassword
  port <- pure $ fromMaybe 3000 config.port
  hostname <- pure $ fromMaybe "0.0.0.0" config.hostname
  let serverOptions = { hostname, port }
  liftEffect $
    Server.run
      serverOptions
      (onListen { hostname, port })
      (onRequest auth context)
