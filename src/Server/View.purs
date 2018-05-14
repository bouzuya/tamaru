module Server.View
  ( View(..)
  ) where

import Bouzuya.HTTP.Server.Node (Request)
import Data.Foldable (intercalate)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Server.Model (Group)

data View
  = GroupListView (Array Group)
  | GroupView Group
  | RequestView Request

instance showView :: Show View where
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