module Client.Fetch.Options
  ( FetchOptions
  , body
  , defaults
  , headers
  , method
  , url
  ) where

import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.Method as Method
import Data.Functor.Contravariant (cmap)
import Data.Options (Option, Options, defaultToOptions, opt)
import Data.StrMap (StrMap)
import Prelude (show)

data FetchOptions

body :: Option FetchOptions String
body = opt "body"

defaults :: Options FetchOptions
defaults = defaultToOptions "method" Method.GET

headers :: Option FetchOptions (StrMap String)
headers = opt "headers"

method :: Option FetchOptions Method
method = cmap show (opt "method")

url :: Option FetchOptions String
url = opt "url"
