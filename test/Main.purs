module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.Server.Action as ServerAction
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  ServerAction.tests
