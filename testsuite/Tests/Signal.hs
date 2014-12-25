{-# LANGUAGE ViewPatterns #-}
module Tests.Signal (
    tests
) where

import Stride
import Signal
import Test.HUnit
import Control.Monad.Trans (lift)
import Control.Monad (forM_, replicateM)
import Control.Applicative

mapTest :: IO ()
mapTest = runReactT $ do
    (x, xI) <- input (5 :: Int)
    let y = (+ 3) <$> x
    (yV, yO) <- output y
    lift $ assertEqual "for initial output," 8 yV
    (end -> yV) <- yO
    lift $ assertEqual "for initial look value," 8 yV
    xI $ set 8
    (end -> yV) <- yO
    lift $ assertEqual "for updated look value," 11 yV
    xI keep
    (end -> yV) <- yO
    lift $ assertEqual "for pseudo-updated look value," 11 yV

deferTest :: IO ()
deferTest = runReactT $ do
    (x, xI) <- input (5 :: Int)
    (y, yI) <- input (8 :: Int)
    (z, zI) <- input x
    let a = defer z
    let b = (* 2) <$> a
    let c = (+ 3) <$> x
    (bV, bO) <- output b
    (cV, cO) <- output c
    lift $ assertEqual "for initial output," 10 bV
    lift $ assertEqual "for initial alternate output," 8 cV
    xI $ set 7
    yI $ set 13
    (end -> bV) <- bO
    lift $ assertEqual "for second output," 14 bV
    zI $ set y
    xI $ set 3
    (end -> cV) <- cO
    lift $ assertEqual "for second alternate output," 6 cV
    (end -> bV) <- bO
    lift $ assertEqual "for third output," 26 bV
    yI $ set 4
    (end -> bV) <- bO
    lift $ assertEqual "for fourth output," 8 bV
    zI $ set x
    yI $ set 12
    (end -> bV) <- bO
    lift $ assertEqual "for last output," 6 bV

stressTest :: IO ()
stressTest = runReactT $ do
    inputs <- replicateM 100 $ input (1 :: Integer)
    let pyramid xs = case xs of
          (x : y : xs) ->
            let z = (+) <$> x <*> y
            in z : pyramid (y : xs)
          _ -> []
    let [final] = iterate pyramid (map fst inputs) !! 99
    (fV, fO) <- output final
    lift $ assertEqual "for initial output," (2 ^ (99 :: Integer)) fV
    snd (inputs !! 1) $ set 2
    (end -> fV) <- fO
    lift $ assertEqual "for second output," (2 ^ (99 :: Integer) + 99) fV
    forM_ inputs (\(_, i) -> i $ set 5)
    (end -> fV) <- fO
    lift $ assertEqual "for third output," (5 * 2 ^ (99 :: Integer)) fV

tests :: Test
tests = test [
    "map" ~: test mapTest,
    "defer" ~: test deferTest,
    "stress" ~: test stressTest]
