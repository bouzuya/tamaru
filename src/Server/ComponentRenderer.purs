module Server.ComponentRenderer (render) where

import Control.Monad.Eff (Eff)
import Prelude (pure)

render :: forall e. Eff e String
render = pure ""
