module Client.Fetch
  ( Effect
  , HTTP
  , fetch
  ) where

import Client.Fetch.Options (FetchOptions, defaults)
import Control.Monad.Aff (Aff)
import Effect (Effect, kind Effect)
import Effect.Class (liftEff)
import Control.Promise (Promise)
import Control.Promise as Promise
import Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Options (Options, options)
import Prelude (append, bind, eq, pure)

type Effect e = (http :: HTTP | e)

foreign import data HTTP :: Effect

data FetchResponse

foreign import fetchImpl
  :: forall e
  . Foreign
  -> Effect (Promise FetchResponse)

foreign import textImpl
  :: forall e
  . FetchResponse
  -> Effect (Promise String)

foreign import statusImpl :: FetchResponse -> Int

fetch' :: forall e. Options FetchOptions -> Aff FetchResponse
fetch' opts = do
  promise <- liftEff (fetchImpl (options (append defaults opts)))
  Promise.toAff promise

fetch
  :: forall e
  . Options FetchOptions
  -> Aff { body :: Maybe String, status :: Int }
fetch opts = do
  response <- fetch' opts
  let status = statusImpl response
  if eq status 204
    then pure { body: Nothing, status }
    else do
      promise <- liftEff (textImpl response)
      b <- Promise.toAff promise
      pure { body: Just b, status }
