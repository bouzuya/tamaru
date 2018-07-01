module Server.Uint8Array
  ( Effect
  , empty
  , fromBuffer
  , fromString
  , toBuffer
  , toString
  ) where

import Control.Monad.Eff (Eff)
import Data.ArrayBuffer.Types (Uint8Array)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Prelude (bind, pure)
import Unsafe.Coerce (unsafeCoerce)

type Effect e = (buffer :: Buffer.BUFFER | e)

empty :: Uint8Array
empty = fromArrayOctet []

fromBuffer :: forall e. Buffer.Buffer -> Eff (Effect e) Uint8Array
fromBuffer b = do
  a' <- Buffer.toArray b
  pure (fromArrayOctet a')

fromString :: forall e. String -> Eff (Effect e) Uint8Array
fromString s = do
  b <- Buffer.fromString s Encoding.UTF8
  fromBuffer b

toBuffer :: forall e. Uint8Array -> Eff (Effect e) Buffer.Buffer
toBuffer a = Buffer.fromArray (toArrayOctet a)

toString :: forall e. Uint8Array -> Eff (Effect e) String
toString a = do
  b <- toBuffer a
  Buffer.toString Encoding.UTF8 b

fromArrayOctet :: Array Buffer.Octet -> Uint8Array
fromArrayOctet = unsafeCoerce

toArrayOctet :: Uint8Array -> Array Buffer.Octet
toArrayOctet = unsafeCoerce