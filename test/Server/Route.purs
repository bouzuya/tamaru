module Test.Server.Route
  ( tests
  ) where

import Prelude

import Bouzuya.HTTP.Method as Method
import Data.Maybe as Maybe
import Server.Action as Action
import Server.Route as Route
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Server.Route" do
  TestUnit.test "Route.route" do
    Assert.equal
      (Maybe.Just Action.GetIndex)
      (Route.route Method.GET [])
    Assert.equal
      (Maybe.Just Action.GetGroupList)
      (Route.route Method.GET ["groups"])
    Assert.equal
      (Maybe.Just (Action.GetGroup "1"))
      (Route.route Method.GET ["groups", "1"])
    Assert.equal
      (Maybe.Just (Action.GetGroupDataList "1"))
      (Route.route Method.GET ["groups", "1", "data"])
    Assert.equal
      (Maybe.Just (Action.UpdateGroupData "1"))
      (Route.route Method.PUT ["groups", "1", "data"])
    Assert.equal
      (Maybe.Just (Action.GetGroupData "1" "2"))
      (Route.route Method.GET ["groups", "1", "data", "2"])
