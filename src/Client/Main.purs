module Client.Main (main) where

import Client.Component.Body as Body
import Control.Bind (bind, pure)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM.HTML.Types (htmlElementToElement, htmlElementToNode)
import DOM.Node.Element (getAttribute)
import DOM.Node.Node (firstChild, removeChild)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (maybe)
import Data.Unit (Unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (discard, ($))

loadInitialState :: forall e. Aff (HA.HalogenEffects e) String
loadInitialState = do
  stateElement' <- HA.selectElement (QuerySelector "script")
  stateElement <- maybe
    (throwError (error "Could not find script"))
    pure
    stateElement'
  initialStateStringMaybe <- liftEff $ getAttribute
    "tamaru-data"
    (htmlElementToElement stateElement)
  initialStateString <- maybe
    (throwError (error "initialState is not found"))
    pure
    initialStateStringMaybe
  pure initialStateString -- TODO: parse

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  body' <- HA.selectElement (QuerySelector ".body")
  body <- maybe (throwError (error "Could not find .body")) pure body'
  child' <- liftEff (firstChild (htmlElementToNode body))
  child <- maybe (throwError (error "no child")) pure child'
  _ <- liftEff (removeChild child (htmlElementToNode body))
  initialStateString <- loadInitialState -- TODO
  liftEff (log initialStateString) -- TODO: remote
  runUI Body.body { groupList: [] } body
