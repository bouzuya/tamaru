module Server.View
  ( Effect
  , View(..)
  , toUint8Array
  ) where

import Bouzuya.HTTP.Request (Request)
import Common.Model (Group, Data)
import Effect (Effect)
import Data.Argonaut as Json
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=))
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.StrMap as StrMap
import Prelude (pure, show)
import Server.Uint8Array as Uint8Array

type Effect e = Uint8Array.Effect e

data View
  = DataListView (Array Data)
  | DataView Data
  | GroupListView (Array Group)
  | GroupView Group
  | IndexView String
  | RequestView Request
  | StaticView Uint8Array

instance encodeJsonView :: EncodeJson View where
  encodeJson (DataListView xs) =
    encodeJson $ DataView <$> xs
  encodeJson (DataView x) =
    encodeJson $
      StrMap.fromFoldable
        [ "id" := x.id
        , "value" := x.value
        ]
  encodeJson (GroupListView xs) =
    encodeJson $ GroupView <$> xs
  encodeJson (GroupView x) =
    encodeJson $
      StrMap.fromFoldable
        [ "id" := x.id
        ]
  encodeJson (IndexView s) = encodeJson s
  encodeJson (RequestView r) =
    encodeJson $
      StrMap.fromFoldable
        [ "method" := show r.method
        , "pathname" := r.pathname
        , "query" := encodeJson r.searchParams
        , "body" := r.body
        ]
  encodeJson (StaticView _) = encodeJson ""

toUint8Array :: forall e. View -> Effect Uint8Array
toUint8Array (IndexView s) = Uint8Array.fromString s
toUint8Array (StaticView b) = pure b
toUint8Array view = do
  let s = Json.stringify $ encodeJson view
  Uint8Array.fromString s
