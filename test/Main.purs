module Test.Main (main) where

import Bouzuya.Halogen.StringRenderer (render)
import Client.Component.App (app)
import Client.Component.GroupList (groupList)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Foldable (intercalate)
import Prelude (Unit, discard, unit)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main
  :: forall e
  . Eff
    ( avar :: AVAR
    , console :: CONSOLE
    , dom :: DOM
    , exception :: EXCEPTION
    , ref :: REF
    , testOutput :: TESTOUTPUT
    | e
    )
    Unit
main = runTest do
  suite "Bouzuya.Halogen.StringRenderer" do
    test "app" do
      Assert.equal
        (intercalate ""
          [ "<html>"
          , "<head><title>tamaru</title></head>"
          , "<body>"
          , "<header><h1>tamaru</h1></header>"
          , "<div class=\"body\">"
          , "<p>body</p>"
          , "<select class=\"group-list\"></select>"
          , "</div>"
          , "<footer>bouzuya</footer>"
          , "</body>"
          , "</html>"
          ]
        )
        (render app unit)
    test "groupList (empty)" do
      Assert.equal
        (intercalate ""
          [ "<select class=\"group-list\">"
          , "</select>"
          ]
        )
        (render groupList { groupList: [] })
    test "groupList (not empty)" do
      Assert.equal
        (intercalate ""
          [ "<select class=\"group-list\">"
          , "<option class=\"group-list-item\" selected value=\"1\">1</option>"
          , "</select>"
          ]
        )
        (render groupList { groupList: [{ id: "1", data: [] }] })
