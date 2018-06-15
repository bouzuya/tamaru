module Client.Main (main) where

import Client.Component.Body as Body
import Control.Bind (bind, pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM.HTML.Types (htmlElementToNode)
import DOM.Node.Node (firstChild, removeChild)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (maybe)
import Data.Unit (Unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (discard)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  body' <- HA.selectElement (QuerySelector ".body")
  body <- maybe (throwError (error "Could not find .body")) pure body'
  child' <- liftEff (firstChild (htmlElementToNode body))
  child <- maybe (throwError (error "no child")) pure child'
  _ <- liftEff (removeChild child (htmlElementToNode body))
  runUI Body.body { groupList: [] } body
