{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
module Rand (
    Rand,
    runRand,
    evalRand,
    roll
) where

import qualified BinaryUtils as U
import Data.Word (Word8)
import Data.Bits
import "mtl" Control.Monad.State (State, runState, evalState, get, put)
import Control.Applicative

-- | A procedure that produces a random value from a stream of random bytes.
newtype Rand a = Rand (State [Word8] a) deriving (Functor, Applicative, Monad)

-- | Runs a random procedure.
runRand :: Rand a -> [Word8] -> (a, [Word8])
runRand (Rand x) = runState x

-- | Runs a random procedure, returning only the result.
evalRand :: Rand a -> [Word8] -> a
evalRand (Rand x) = evalState x

-- | Gets the next byte from the random stream (a random byte).
nextByte :: Rand Word8
nextByte = Rand $ do
    (x : xs) <- get
    put xs
    return x

-- | A random procedure which gets a natural number up to, but not including,
-- the given integer.
roll :: Integer -> Rand Integer
roll i = go where
    size' 0 = 0
    size' i = 1 + size' (i `shiftR` 8)
    size = size' i
    q = (1 `shiftL` (8 * size)) :: Integer
    m = q - (q `mod` i)
    getBytes n = mapM (const nextByte) [0 .. (n - 1)]
    go = do
        bytes <- getBytes size
        let r = U.roll bytes
        if r < m then return (r `mod` i) else go
