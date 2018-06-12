module Client.Main (main) where

import Client.Component.App (app)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI app { groupList: [] } body -- TODO
