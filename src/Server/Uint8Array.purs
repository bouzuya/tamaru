module Server.Uint8Array
  ( Effect
  , fromString
  , toString
  ) where

import Control.Monad.Eff (Eff)
import Data.ArrayBuffer.Types (Uint8Array)
import Node.Buffer (BUFFER, Octet)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Prelude (bind, pure)
import Unsafe.Coerce (unsafeCoerce)

type Effect e = (buffer :: BUFFER | e)

fromString :: forall e. String -> Eff (Effect e) Uint8Array
fromString s = do
  b <- Buffer.fromString s Encoding.UTF8
  a' <- Buffer.toArray b
  let
    a :: Uint8Array
    a = unsafeCoerce a'
  pure a

toString :: forall e. Uint8Array -> Eff (Effect e) String
toString a = do
  let
    a' :: Array Octet
    a' = unsafeCoerce a
  b <- Buffer.fromArray a'
  Buffer.toString Encoding.UTF8 b
