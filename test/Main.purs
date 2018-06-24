module Test.Main (main) where

import Bouzuya.Halogen.StringRenderer (render)
import Client.Component.DataList (dataList)
import Client.Component.GroupList (groupList)
import Client.Component.ServerRoot (serverRoot)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Foldable (intercalate)
import Prelude (Unit, discard)
import Test.DayOfYear as DayOfYear
import Test.DateTime as DateTime
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
  DateTime.tests
  DayOfYear.tests
  suite "Bouzuya.Halogen.StringRenderer" do
    test "ServerRoot" do
      Assert.equal
        (intercalate ""
          [ "<html>"
          , "<head><title>tamaru</title></head>"
          , "<body>"
          , "<header><h1>tamaru</h1></header>"
          , "<div class=\"body\">"
          , "<div class=\"body\">"
          , "<p>body</p>"
          , "<select class=\"group-list\"></select>"
          , "<div class=\"data-input\">"
          , "<form>"
          , "<label class=\"value\">"
          , "<span class=\"label\">value</span>"
          , "<span class=\"value\"><input name=\"value\" value=\"\"/></span>"
          , "</label>"
          , "<button type=\"submit\">OK</button>"
          , "</form>"
          , "<span class=\"output\"></span>"
          , "</div>"
          , "<ul class=\"data-list\"></ul>"
          , "</div>"
          , "</div>"
          , "<footer>bouzuya</footer>"
          , "<script data-initial-state=\"{&quot;groupList&quot;:[]}\" src=\"&#x2F;scripts&#x2F;index.js\"></script>"
          , "</body>"
          , "</html>"
          ]
        )
        (render serverRoot { groupList: [] })
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
    test "dataList (empty)" do
      Assert.equal
        (intercalate ""
          [ "<ul class=\"data-list\">"
          , "</ul>"
          ]
        )
        (render dataList { dataList: [] })
    test "dataList (not empty)" do
      Assert.equal
        (intercalate ""
          [ "<ul class=\"data-list\">"
          , "<li class=\"data-list-item\">"
          , "<span class=\"id\">1</span>"
          , "<span class=\"value\">a</span>"
          , "</li>"
          , "</ul>"
          ]
        )
        (render dataList { dataList: [{ id: "1", value: "a" }] })
