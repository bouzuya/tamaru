module Server.Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(..))
import Prelude (Unit, pure)
import Server.Node.Server (HTTP, StatusCode(..), run)

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = do
  run (log "listening...") do
    pure
      { body: "OK"
      , header: (Tuple "Content-Type" "text/plain")
      , status: (StatusCode 200)
      }

