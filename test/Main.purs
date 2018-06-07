module Test.Main (main) where

import Client.Component.App (app)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Prelude (Unit, bind, (<>))
import Server.ComponentRenderer (renderAsString)
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
  suite "ComponentRenderer" do
    test "app" do
      s <- renderAsString app
      Assert.equal
        s
        ( "<html>"
        <> "<head><title>tamaru</title></head>"
        <> "<body>"
        <> "<header><h1>tamaru</h1></header>"
        <> "<div class=\"body\"><p>body</p></div>"
        <> "<footer>bouzuya</footer>"
        <> "</body>"
        <> "</html>"
        )
