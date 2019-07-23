module Server.BasicAuth
  ( isAuthenticated
  ) where

import Prelude

import Bouzuya.HTTP.Headers as Headers
import Bouzuya.HTTP.Request (Request)
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Effect (Effect)
import Node.Buffer as Buffer
import Node.Encoding as Encoding

isAuthenticated ::
  { userid :: String, password :: String } -> Effect (Request -> Boolean)
isAuthenticated { userid, password } = do
  encoded <- base64encode (userid <> ":" <> password)
  pure
    (\{ headers } ->
      eq
        (Maybe.Just ("Basic " <> encoded))
        (map Tuple.snd (Headers.lookup "authorization" headers)))

-- private

base64encode :: String -> Effect String
base64encode s = do
  b <- Buffer.fromString s Encoding.UTF8
  Buffer.toString Encoding.Base64 b
