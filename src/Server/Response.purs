module Server.Response
  ( response200
  , response302
  , response400
  , response401
  , response404
  , response500
  ) where

import Prelude

import Bouzuya.HTTP.Header (Header)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode (status200, status302, status400, status401, status404, status500)
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Server.Uint8Array as Uint8Array
import Server.View (View, toUint8Array)

-- Header helpers

contentType :: String -> Header
contentType = Tuple "Content-Type"

location :: String -> Header
location = Tuple "Location"

-- Response helpers

response200 :: String -> View -> Effect Response
response200 viewType view = do
  body <- toUint8Array view
  pure
    { body
    , headers: [contentType viewType]
    , status: status200
    }

response302 :: String -> Response
response302 loc =
  { body: Uint8Array.empty
  , headers: [location loc]
  , status: status302
  }

response400 :: Response
response400 =
  { body: Uint8Array.empty
  , headers: [contentType "text/plain"]
  , status: status400
  }

response401 :: Response
response401 =
  { body: Uint8Array.empty
  , headers:
    [ contentType "text/plain"
    , Tuple "WWW-Authenticate" "Basic"
    ]
  , status: status401
  }

response404 :: Response
response404 =
  { body: Uint8Array.empty
  , headers: [contentType "text/plain"]
  , status: status404
  }

response500 :: Response
response500 =
  { body: Uint8Array.empty
  , headers: [contentType "text/plain"]
  , status: status500
  }
