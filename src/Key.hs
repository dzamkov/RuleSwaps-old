{-# LANGUAGE OverloadedStrings #-}
module Key (
    Key,
    length,
    hashData,
    hash,
    merge,
    split,
    fromInteger,
    toInteger,
    encrypt,
    decrypt,
    toRandStream,
    rand
) where

import Prelude hiding (length, fromInteger, toInteger)
import BinaryUtils
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA256 as SHA
import System.Entropy (getEntropy)

-- | A cryptographically secure key that can be used for random number
-- generation and arbitration.
newtype Key = Key ByteString deriving (Eq, Ord, Show)

-- | The number of bytes in a key.
length :: Int
length = 32

-- | Hashes a 'ByteString' to get a key. This is cryptographically secure.
hashData :: ByteString -> Key
hashData = Key . SHA.hash

-- | Hashes a key to get another key. This is cryptographically secure.
hash :: Key -> Key
hash (Key input) = hashData input

-- | Merges two keys into a single key. This is not cryptographically secure
-- It is possible to recover one of the input keys given the other input key
-- and the output key. This is a commutative and associative operation.
merge :: Key -> Key -> Key
merge (Key a) (Key b) = Key $ B.pack $ B.zipWith xor a b

-- | Splits a key into two keys. This is not cryptographically secure. This can
-- be undone using 'merge'.
split :: Key -> (Key, Key)
split (Key a) = (Key l, Key r) where
    (l', r') = B.splitAt (length `div` 2) a
    l = B.append l' (B.replicate (length `div` 2) 0)
    r = B.append (B.replicate (length `div` 2) 0) r'

-- | Converts a key into an integer.
toInteger :: Key -> Integer
toInteger (Key a) = runGetStrict (getFixedInteger length) a

-- | Converts an integer into a key.
fromInteger :: Integer -> Key
fromInteger i = Key $ runPutStrict $ putFixedInteger length i

-- | Uses a key to encrypt a ByteString.
encrypt :: Key -> ByteString -> ByteString
encrypt _ x = x -- TODO: improve security

-- | Uses a key to decrypt a ByteString.
decrypt :: Key -> ByteString -> ByteString
decrypt _ x = x

-- | Converts a key into a stream of random bytes. The key must already be
-- semi-random and the key may be reconstructed from the stream.
toRandStream :: Key -> [Word8]
toRandStream k@(Key a) = B.unpack a ++ toRandStream (hash k)
    -- TODO: make better

-- | Generates a random key
rand :: IO Key
rand = fmap Key (getEntropy length)
