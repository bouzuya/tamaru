module Client.Main (main) where

import Common.Component.ClientRoot as ClientRoot
import Common.Model (Group, Data)
import Control.Bind (bind, pure)
import Control.Monad.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEff)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM.HTML.Types (htmlElementToElement, htmlElementToNode)
import DOM.Node.Element (getAttribute)
import DOM.Node.Node (firstChild, removeChild)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Argonaut (Json, decodeJson, jsonParser, (.?))
import Data.Either (Either, either, hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Unit (Unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (const, discard, ($))

type Effect e = HA.HalogenEffects (ClientRoot.Effect e)

loadInitialState
  :: forall e
  . Aff (Maybe { groupList :: Array Group })
loadInitialState = do
  stateElement' <- HA.selectElement (QuerySelector "script")
  stateElement <- maybe
    (throwError (error "Could not find script"))
    pure
    stateElement'
  initialStateStringMaybe <- liftEff $ getAttribute
    "data-initial-state"
    (htmlElementToElement stateElement)
  initialStateString <- maybe
    (throwError (error "initialState is not found"))
    pure
    initialStateStringMaybe
  pure $ fromString initialStateString
  where
    fromString :: String -> Maybe { groupList :: Array Group }
    fromString s = hush do
      json <- jsonParser s
      o <- decodeJson json
      a' <- o .? "groupList"
      groupList <- traverse decodeJsonGroup a'
      pure { groupList }
      where
        decodeJsonGroup :: Json -> Either String Group
        decodeJsonGroup json = do
          o <- decodeJson json
          a' <- o .? "data"
          d <- traverse decodeJsonData a'
          id <- o .? "id"
          pure { data: d, id }
        decodeJsonData :: Json -> Either String Data
        decodeJsonData json = do
          o <- decodeJson json
          id <- o .? "id"
          value <- o .? "value"
          pure { id, value }

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  body' <- HA.selectElement (QuerySelector ".body")
  body <- maybe (throwError (error "Could not find .body")) pure body'
  child' <- liftEff (firstChild (htmlElementToNode body))
  child <- maybe (throwError (error "no child")) pure child'
  _ <- liftEff (removeChild child (htmlElementToNode body))
  initialState' <- loadInitialState
  initialState <- maybe (throwError (error "no state")) pure initialState'
  runUI ClientRoot.clientRoot initialState body
