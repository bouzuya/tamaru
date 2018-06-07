module Server.ComponentRenderer (renderAsString) where

import Client.Component.App (app)
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, readVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, newRef)
import Data.Maybe (Maybe)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Driver as HAD
import Halogen.Aff.Driver.State as HADS
import Halogen.HTML as HH
import Halogen.Query.InputF as HQI
import Halogen.VDom as HV
import Halogen.VDom.DOM.Prop as HVDP
import Halogen.VDom.DOM.StringRenderer as VSR
import Prelude (Unit, bind, const, id, pure, unit, ($))
import Unsafe.Coerce (unsafeCoerce)

type Child f g p e =
  H.ComponentSlot HH.HTML g (Aff (HA.HalogenEffects e)) p (f Unit)

type ChildRenderer f g p e =
  Child f g p e
  -> Eff (HA.HalogenEffects e) (HADS.RenderStateX RenderState e)

type QueryHandler f e =
  forall x. HQI.InputF x (f x) -> Eff (HA.HalogenEffects e) Unit

type VHTML f g p e =
  HV.VDom (Array (HVDP.Prop (HQI.InputF Unit (f Unit)))) (Child f g p e)

newtype RenderState s f g p o e =
  RenderState
    { machine :: HV.Step (Eff (HA.HalogenEffects e)) (VHTML f g p e) Unit
    , renderChildRef :: Ref (ChildRenderer f g p e)
    }

renderAsString :: forall e. Aff (HA.HalogenEffects e) String
renderAsString = do
  var <- makeEmptyVar
  _ <- HAD.runUI (renderSpec var) app unit
  readVar var

renderSpec :: forall e. AVar String -> HAD.RenderSpec HH.HTML RenderState e
renderSpec var =
  { render: render var
  , renderChild: id
  , removeChild: const (pure unit)
  }

render
  :: forall s f g p o e
  . AVar String
  -> QueryHandler f e
  -> ChildRenderer f g p e
  -> HH.HTML (Child f g p e) (f Unit)
  -> Maybe (RenderState s f g p o e)
  -> Eff (HA.HalogenEffects e) (RenderState s f g p o e)
render var _ child (HH.HTML vdom) _ = do
  renderChildRef <- newRef child
  let s = VSR.render (const "") vdom
  _ <- runAff_ (const (pure unit)) (putVar s var)
  pure $ RenderState
    { machine: unsafeCoerce unit -- FIXME
    , renderChildRef
    }
