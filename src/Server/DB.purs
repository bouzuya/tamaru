module Server.DB
  ( DB
  , Data
  , DataId
  , DataValue
  , Group
  , GroupId
  , db
  , findGroupAll
  , findGroupById
  ) where

import Data.Eq ((==))
import Data.Foldable (find)
import Data.Maybe (Maybe)

type DB = Array Group
type GroupId = String -- "[a-z][0-9a-z_]"
type DataId = String -- "YYYY-MM-DD"
type DataValue = String -- "\d+(\.\d+)?"
type Group =
  { id :: GroupId
  , data :: Array Data
  }
type Data =
  { id :: DataId
  , value :: DataValue
  }

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
findGroupAll db = db

findGroupById :: DB -> GroupId -> Maybe Group
findGroupById db groupId = find (\{ id } -> id == groupId) db
