module Server.Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit)
import Server.Node.Server (HTTP, run)

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = do
  run (log "listening...")
