module Server.DB
  ( Context
  , DB
  , db
  , findDataAllByGroupId
  , findDataByGroupIdAndDataId
  , findGroupAll
  , findGroupById
  ) where

import Control.Bind ((<$>), (>>=))
import Control.Monad.Eff.AVar (AVar)
import Data.Eq ((==))
import Data.Foldable (find)
import Data.Maybe (Maybe)
import Server.Model (Data, Group, GroupId, DataId)

type Context = AVar DB
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

findDataAllByGroupId :: DB -> GroupId -> Maybe (Array Data)
findDataAllByGroupId d groupId = _.data <$> (findGroupById d groupId)

findDataByGroupIdAndDataId :: DB -> GroupId -> DataId -> Maybe Data
findDataByGroupIdAndDataId d groupId dataId =
  (findDataAllByGroupId d groupId) >>= (find (\{ id } -> id == dataId))

findGroupAll :: DB -> Array Group
findGroupAll d = d

findGroupById :: DB -> GroupId -> Maybe Group
findGroupById d groupId = find (\{ id } -> id == groupId) d
