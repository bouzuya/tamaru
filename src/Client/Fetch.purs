module Client.Fetch
  ( Effect
  , HTTP
  , fetch
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Foreign (Foreign)
import Data.Options (Options, options)
import Fetch.Options (FetchOptions, defaults)
import Prelude (append, bind)

type Effect e = (http :: HTTP | e)

foreign import data HTTP :: Effect

foreign import fetchImpl
  :: forall e
  . Foreign
  -> Eff (Effect e) (Promise Foreign)

fetch :: forall e. Options FetchOptions -> Aff (Effect e) Foreign
fetch opts = do
  promise <- liftEff (fetchImpl (options (append defaults opts)))
  Promise.toAff promise
