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
  -> Eff
    (StaticEff e)
    (Maybe
      { dataAsString :: String
      , extension :: String
      , localPath :: String
      , path :: String
      })
staticRoute dir path
  | Path.isAbsolute (Path.normalize path) = runMaybeT do
      let normalizedPath = Path.normalize path
      let extension = Path.extname normalizedPath
      let localPath = Path.concat [dir, normalizedPath]
      exists <- lift $ FS.exists localPath
      _ <- MaybeT $ pure $ guard exists (Just localPath)
      stat <- lift $ FS.stat localPath
      fullPath <- MaybeT $ pure $ guard (Stats.isFile stat) (Just localPath)
      buffer <- lift $ FS.readFile fullPath
      dataAsString <- lift $ Buffer.toString Encoding.UTF8 buffer
      pure { dataAsString, extension, localPath, path: normalizedPath }
  | otherwise = pure Nothing
