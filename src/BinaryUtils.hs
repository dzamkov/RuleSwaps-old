module BinaryUtils (
    putFixedList,
    getFixedList,
    unroll,
    roll,
    putFixedInteger,
    getFixedInteger,
    runPutStrict,
    runGetStrict
) where

import Data.Bits
import Data.Binary
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Control.Applicative

-- Reimplementing the things in Data.Binary.Class that weren't exported.
-- Most code is from there.

-- | Writes a fixed-size list.
putFixedList :: (Binary a) => Int -> [a] -> Put
putFixedList 0 _ = return ()
putFixedList n (x : xs) = put x >> putFixedList (n - 1) xs
putFixedList _ [] = undefined

-- | Reads a fixed-size list.
getFixedList :: (Binary a) => Int -> Get [a]
getFixedList 0 = return []
getFixedList n = go [] n where
    go xs 0 = return $! reverse xs
    go xs n = do
        x <- get
        x `seq` go (x : xs) (n - 1)

-- | Convert an integer into bytes.
unroll :: Integer -> [Word8]
unroll i = fromInteger i : unroll (i `shiftR` 8)

-- | Converts bytes into an integer.
roll :: [Word8] -> Integer
roll = foldr (\b a -> a `shiftL` 8 .|. toInteger b) 0

-- | Writes a fixed-size integer.
putFixedInteger :: Int -> Integer -> Put
putFixedInteger n = putFixedList n . unroll

--- | Reads a fixed-size integer.
getFixedInteger :: Int -> Get Integer
getFixedInteger n = roll <$> getFixedList n

-- | Runs a 'Put' procedure and returns a strict 'ByteString'.
runPutStrict :: Put -> ByteString
runPutStrict = toStrict . runPut

-- | Runs a 'Get' procedure and returns a strict 'ByteString'.
runGetStrict :: Get a -> ByteString -> a
runGetStrict x = runGet x . fromStrict
