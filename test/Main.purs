module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.Server.Action as ServerAction
import Test.Server.BasicAuth as ServerBasicAuth
import Test.Server.Route as ServerRoute
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  ServerAction.tests
  ServerBasicAuth.tests
  ServerRoute.tests
