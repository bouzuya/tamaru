module Bouzuya.Halogen.StringRenderer (render) where

import Halogen as H
import Halogen.HTML as HH
import Halogen.VDom.DOM.StringRenderer as VSR
import Prelude (Unit)

render
  :: forall f i o m
  . H.Component HH.HTML f i o m
  -> i
  -> String
render = componentToString

componentToString
  :: forall f i o m
  . H.Component HH.HTML f i o m
  -> i
  -> String
componentToString component input =
  H.unComponent
    (\{ initialState, render: render' } ->
      let (HH.HTML vdom) = render' (initialState input) in
      VSR.render componentSlotToString vdom
    )
    component

componentSlotToString
  :: forall f g m p
  . H.ComponentSlot HH.HTML g m p (f Unit)
  -> String
componentSlotToString slot =
  H.unComponentSlot
    (\_ component input _ _ _ -> componentToString component input)
    slot
