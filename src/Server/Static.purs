module Server.Static
  ( Effect
  , staticRoute
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path as Path
import Prelude (bind, otherwise, pure, ($))
import Server.Uint8Array as Uint8Array

type Effect e =
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
    (Effect e)
    (Maybe
      { binary :: Uint8Array
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
      binary <- lift $ Uint8Array.fromBuffer buffer
      pure { binary, extension, localPath, path: normalizedPath }
  | otherwise = pure Nothing
