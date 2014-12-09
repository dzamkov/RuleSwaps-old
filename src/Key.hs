{-# LANGUAGE OverloadedStrings #-}
module Key (
    Key,
    length,
    hash,
    merge,
    split,
    roll,
    rand
) where

import Prelude hiding (length)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Crypto.Hash.SHA256 as SHA
import System.Entropy (getEntropy)

-- | A cryptographically secure key that can be used for random number
-- generation and arbitration.
newtype Key = Key ByteString deriving (Eq, Ord, Show)

-- | The number of bytes in a key.
length :: Int
length = 32

-- | Hashes a key to get another key. This is cryptographically secure. It is
-- not feasible to recover the input key from the output key.
hash :: Key -> Key
hash (Key input) = Key $ SHA.hash input

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

-- | Uses a key to select a "random" natural number less than the given
-- number.
roll :: Integer -> Key -> Integer
roll n (Key a) = res where
    bytes = B.unpack a
    i = List.foldl (\i n -> 256 * i + toInteger n) 0 bytes
    res = i `mod` n

-- | Generates a random key
rand :: IO Key
rand = fmap Key (getEntropy length)
