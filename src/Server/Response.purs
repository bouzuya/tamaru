module Server.Response
  ( response200
  , response302
  , response404
  ) where


import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode (status200, status302, status404)
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Server.View (View)

response200 :: View -> Response
response200 view =
  { body: show view
  , headers: [(Tuple "Content-Type" "text/plain")]
  , status: status200
  }

response302 :: String -> Response
response302 location =
  { body: ""
  , headers: [Tuple "Location" location]
  , status: status302
  }

response404 :: Response
response404 =
  { body: ""
  , headers: [(Tuple "Content-Type" "text/plain")]
  , status: status404
  }
