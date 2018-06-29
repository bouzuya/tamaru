module Common.Model
  ( Data
  , DataId
  , DataValue
  , Group
  , GroupId
  ) where

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
