module Server.Response
  ( response200
  , response302
  , response400
  , response404
  , response500
  ) where

import Bouzuya.HTTP.Header (Header)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode (status200, status302, status400, status404, status500)
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Server.View (View)

-- Header helpers

contentType :: String -> Header
contentType = Tuple "Content-Type"

location :: String -> Header
location = Tuple "Location"

-- Response helpers

response200 :: View -> Response
response200 view =
  { body: show view
  , headers: [contentType "text/plain"]
  , status: status200
  }

response302 :: String -> Response
response302 loc =
  { body: ""
  , headers: [location loc]
  , status: status302
  }

response400 :: Response
response400 =
  { body: ""
  , headers: [contentType "text/plain"]
  , status: status400
  }

response404 :: Response
response404 =
  { body: ""
  , headers: [contentType "text/plain"]
  , status: status404
  }

response500 :: Response
response500 =
  { body: ""
  , headers: [contentType "text/plain"]
  , status: status500
  }
