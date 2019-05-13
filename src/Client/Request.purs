module Client.Request
  ( addData
  ) where

import Prelude

import Bouzuya.HTTP.Method as Method
import Client.Fetch as Fetch
import Client.Fetch.Options as FetchOptions
import Common.Model (Data, GroupId)
import Effect.Aff (Aff)
import Data.Options ((:=))

addData :: GroupId -> Data -> Aff Unit
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
