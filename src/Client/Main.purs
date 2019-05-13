module Client.Main (main) where

import Prelude

import Common.Component.ClientRoot as ClientRoot
import Common.Model (Group, Data)
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, decodeJson, jsonParser, (.:))
import Data.Traversable (traverse)
import Data.Either (Either)
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..))

loadInitialState :: Aff (Maybe { groupList :: Array Group })
loadInitialState = do
  stateElement' <- HA.selectElement (QuerySelector "script")
  stateElement <-
    Maybe.maybe
      (throwError (error "Could not find script"))
      pure
      stateElement'
  initialStateStringMaybe <-
    liftEffect $ Element.getAttribute
      "data-initial-state"
      (HTMLElement.toElement stateElement)
  initialStateString <-
    Maybe.maybe
      (throwError (error "initialState is not found"))
      pure
      initialStateStringMaybe
  pure $ fromString initialStateString
  where
    fromString :: String -> Maybe { groupList :: Array Group }
    fromString s = Either.hush do
      json <- jsonParser s
      o <- decodeJson json
      a' <- o .: "groupList"
      groupList <- traverse decodeJsonGroup a'
      pure { groupList }
      where
        decodeJsonGroup :: Json -> Either String Group
        decodeJsonGroup json = do
          o <- decodeJson json
          a' <- o .: "data"
          d <- traverse decodeJsonData a'
          id <- o .: "id"
          pure { data: d, id }
        decodeJsonData :: Json -> Either String Data
        decodeJsonData json = do
          o <- decodeJson json
          id <- o .: "id"
          value <- o .: "value"
          pure { id, value }

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  body' <- HA.selectElement (QuerySelector ".body")
  body <- Maybe.maybe (throwError (error "Could not find .body")) pure body'
  child' <- liftEffect (Node.firstChild (HTMLElement.toNode body))
  child <- Maybe.maybe (throwError (error "no child")) pure child'
  _ <- liftEffect (Node.removeChild child (HTMLElement.toNode body))
  initialState' <- loadInitialState
  initialState <- Maybe.maybe (throwError (error "no state")) pure initialState'
  runUI ClientRoot.clientRoot initialState body
