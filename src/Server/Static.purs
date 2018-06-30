module Server.Static
  ( StaticEff
  , staticRoute
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS (FS)
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path as Path
import Prelude (bind, otherwise, pure, ($))

type StaticEff e =
  ( buffer :: BUFFER
  , exception :: EXCEPTION
  , fs :: FS
  | e
  )

staticRoute
  :: forall e
  . String
  -> String
  -> Eff (StaticEff e) (Maybe String)
staticRoute dir path
  | Path.isAbsolute path = runMaybeT do
      let localPath = Path.concat [dir, path]
      exists <- lift $ FS.exists localPath
      _ <- MaybeT $ pure $ guard exists (Just localPath)
      stat <- lift $ FS.stat localPath
      fullPath <- MaybeT $ pure $ guard (Stats.isFile stat) (Just localPath)
      buffer <- lift $ FS.readFile fullPath
      lift $ Buffer.toString Encoding.UTF8 buffer
  | otherwise = pure Nothing
