module Server.View
  ( View(..)
  ) where

import Bouzuya.HTTP.Request (Request)
import Data.Foldable (intercalate)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Server.Model (Group, Data)

data View
  = DataListView (Array Data)
  | GroupListView (Array Group)
  | GroupView Group
  | RequestView Request

instance showView :: Show View where
  show (DataListView allData) =
    "[" <> (intercalate "," $ showData <$> allData) <> "]"
    where
      showData { id, value } =
        "{\"id\":\"" <> id <> "\",\"value\":\"" <> value <> "\"}"
  show (GroupListView groups) =
    "[" <> (intercalate "," $ (\{ id } -> show id) <$> groups) <> "]"
  show (GroupView group) =
    "{\"id\":\"" <> group.id <> "\"}"
  show (RequestView { body, headers, method, pathname, searchParams }) =
    intercalate ", "
      [ "method: " <> show method
      , "pathname: " <> pathname
      , "query: " <> (intercalate "," (show <$> searchParams))
      , "body: " <> body
      ]
