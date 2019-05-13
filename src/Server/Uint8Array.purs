module Server.Uint8Array
  ( empty
  , fromBuffer
  , fromString
  , toBuffer
  , toString
  ) where

import Prelude

import Effect (Effect)
import Data.ArrayBuffer.Types (Uint8Array)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Unsafe.Coerce (unsafeCoerce)

empty :: Uint8Array
empty = fromArrayOctet []

fromBuffer :: Buffer.Buffer -> Effect Uint8Array
fromBuffer b = do
  a' <- Buffer.toArray b
  pure (fromArrayOctet a')

fromString :: String -> Effect Uint8Array
fromString s = do
  b <- Buffer.fromString s Encoding.UTF8
  fromBuffer b

toBuffer :: Uint8Array -> Effect Buffer.Buffer
toBuffer a = Buffer.fromArray (toArrayOctet a)

toString :: Uint8Array -> Effect String
toString a = do
  b <- toBuffer a
  Buffer.toString Encoding.UTF8 b

fromArrayOctet :: Array Buffer.Octet -> Uint8Array
fromArrayOctet = unsafeCoerce

toArrayOctet :: Uint8Array -> Array Buffer.Octet
toArrayOctet = unsafeCoerce
