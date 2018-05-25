module Server.View
  ( View(..)
  ) where

import Bouzuya.HTTP.Request (Request)
import Data.Argonaut (jsonNull)
import Data.Argonaut as Json
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (class Show, show)
import Data.StrMap as StrMap
import Server.Model (Group, Data)

data View
  = DataListView (Array Data)
  | DataView Data
  | GroupListView (Array Group)
  | GroupView Group
  | IndexView
  | RequestView Request

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
  encodeJson IndexView = jsonNull
  encodeJson (RequestView r) =
    encodeJson $
      StrMap.fromFoldable
        [ "method" := show r.method
        , "pathname" := r.pathname
        , "query" := encodeJson r.searchParams
        , "body" := r.body
        ]

instance showView :: Show View where
  show IndexView = ""
  show view = Json.stringify $ encodeJson view
