module Server.ComponentRenderer (renderAsString) where

import Control.Monad.Aff (Aff)
import Halogen (unComponent, unComponentSlot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.DOM.StringRenderer as VSR
import Prelude (Unit, pure)

renderAsString
  :: forall e f i o
  . H.Component HH.HTML f i o (Aff (HA.HalogenEffects e))
  -> i
  -> Aff (HA.HalogenEffects e) String
renderAsString component input = do
  pure (componentToString component input)

componentToString
  :: forall e f i o
  . H.Component HH.HTML f i o (Aff (HA.HalogenEffects e))
  -> i
  -> String
componentToString component input =
  unComponent
    (\{ initialState, render } ->
      let (HH.HTML vdom) = render (initialState input) in
      VSR.render componentSlotToString vdom
    )
    component

componentSlotToString
  :: forall e f g p
  . H.ComponentSlot HH.HTML g (Aff (HA.HalogenEffects e)) p (f Unit)
  -> String
componentSlotToString slot =
  unComponentSlot
    (\_ component input _ _ _ -> componentToString component input)
    slot
