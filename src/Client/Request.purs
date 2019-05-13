module Client.Request
  ( Effect
  , addData
  ) where

import Bouzuya.HTTP.Method as Method
import Client.Fetch as Fetch
import Client.Fetch.Options as FetchOptions
import Common.Model (Data, GroupId)
import Control.Monad.Aff (Aff)
import Data.Options ((:=))
import Prelude (Unit, bind, pure, show, unit, (<>))

type Effect e = Fetch.Effect e

addData :: forall e. GroupId -> Data -> Aff Unit
addData groupId d = do
  _ <- Fetch.fetch
    ( FetchOptions.defaults
    <> FetchOptions.method := Method.PUT
    <> FetchOptions.url := ("/groups/" <> groupId <> "/data")
    <> FetchOptions.body := encodeJson d
    )
  pure unit
  where
  encodeJson d' =
    "{\"id\":" <> show d'.id <> ",\"value\":" <> show d'.value <> "}"
