module Test.Server.Action
  ( tests
  ) where

import Prelude

import Server.Action as Action
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Server.Action" do
  TestUnit.test "Eq Action" do
    Assert.equal Action.GetIndex Action.GetIndex
    Assert.assert "notEq" (notEq Action.GetIndex Action.GetGroupList)

  TestUnit.test "Show Action" do
    Assert.equal "GetIndex" (show Action.GetIndex)
    Assert.equal "GetGroupList" (show Action.GetGroupList)
    Assert.equal "GetGroup(1)" (show (Action.GetGroup "1"))
    Assert.equal "GetGroupDataList(1)" (show (Action.GetGroupDataList "1"))
    Assert.equal "UpdateGroupData(1)" (show (Action.UpdateGroupData "1"))
    Assert.equal "GetGroupData(1,2)" (show (Action.GetGroupData "1" "2"))
