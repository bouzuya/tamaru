module Client.Fetch
  ( fetch
  ) where

import Prelude

import Client.Fetch.Options (FetchOptions, defaults)
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise (Promise)
import Control.Promise as Promise
import Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Options (Options, options)

data FetchResponse

foreign import fetchImpl :: Foreign -> Effect (Promise FetchResponse)

foreign import textImpl :: FetchResponse -> Effect (Promise String)

foreign import statusImpl :: FetchResponse -> Int

fetch' :: Options FetchOptions -> Aff FetchResponse
fetch' opts = do
  promise <- liftEffect (fetchImpl (options (append defaults opts)))
  Promise.toAff promise

fetch
  :: Options FetchOptions
  -> Aff { body :: Maybe String, status :: Int }
fetch opts = do
  response <- fetch' opts
  let status = statusImpl response
  if eq status 204
    then pure { body: Nothing, status }
    else do
      promise <- liftEffect (textImpl response)
      b <- Promise.toAff promise
      pure { body: Just b, status }
