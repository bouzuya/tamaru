module Server.DB
  ( DB
  , db
  , findGroupAll
  , findGroupById
  ) where

import Data.Eq ((==))
import Data.Foldable (find)
import Data.Maybe (Maybe)
import Server.Model (Group, GroupId)

type DB = Array Group

db :: DB
db =
  [ { id: "group1"
    , data:
      [ { id: "2018-01-01", value: "50.0" }
      , { id: "2018-01-02", value: "50.5" }
      , { id: "2018-01-03", value: "51.0" }
      ]
    }
  , { id: "group2"
    , data:
      [ { id: "2018-01-01", value: "20" }
      , { id: "2018-01-02", value: "21" }
      , { id: "2018-01-03", value: "22" }
      ]
    }
  ]

findGroupAll :: DB -> Array Group
findGroupAll d = d

findGroupById :: DB -> GroupId -> Maybe Group
findGroupById d groupId = find (\{ id } -> id == groupId) d
