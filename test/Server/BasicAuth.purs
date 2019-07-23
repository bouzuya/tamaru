module Test.Server.BasicAuth
  ( tests
  ) where

import Prelude

import Bouzuya.HTTP.Body as Body
import Bouzuya.HTTP.Method as Method
import Data.Tuple as Tuple
import Effect.Class as Class
import Server.Action as Action
import Server.BasicAuth (isAuthenticated)
import Server.BasicAuth as BasicAuth
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Server.BasicAuth" do
  TestUnit.test "isAuthenticated" do
    let credentials = { userid: "Aladdin", password: "open sesame" }
    isAuthenticated <- Class.liftEffect (BasicAuth.isAuthenticated credentials)
    body <- Class.liftEffect (Body.toArray "")
    let
      okHeader =
        Tuple.Tuple "Authorization" "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
      okRequest =
        { body
        , headers: [okHeader]
        , method: Method.GET
        , pathname: "/"
        , remoteAddress: { host: "0.0.0.0", port: 8080 }
        , searchParams: []
        }
    Assert.assert "OK" (isAuthenticated okRequest)
