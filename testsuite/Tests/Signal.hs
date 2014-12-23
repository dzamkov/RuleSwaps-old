module Tests.Signal (
    tests
) where

import Delta
import Signal
import Test.HUnit
import Control.Monad.Trans (lift)
import Control.Monad (forM)
import Control.Applicative

mapTest :: IO ()
mapTest = runReactT $ do
    (x, xI) <- input (5 :: Int)
    let y = (+ 3) <$> x
    (yV, yO) <- output y
    lift $ assertEqual "for initial output," 8 yV
    (yD, yV) <- yO
    lift $ assertEqual "for initial look value," 8 yV
    lift $ assertEqual "for initial look delta," keep yD
    xI $ set 8
    (yD, yV) <- yO
    lift $ assertEqual "for updated look value," 11 yV
    lift $ assertEqual "for updated look delta," (set 11) yD
    xI keep
    (_, yV) <- yO
    lift $ assertEqual "for pseudo-updated look value," 11 yV

deferTest :: IO ()
deferTest = runReactT $ do
    (x, xI) <- input (5 :: Int)
    (y, yI) <- input (8 :: Int)
    (z, zI) <- input x
    let a = defer z
    let b = cache ((* 2) <$> a)
    let c = cache ((+ 3) <$> x)
    (bV, bO) <- output b
    (cV, cO) <- output c
    lift $ assertEqual "for initial output," 10 bV
    lift $ assertEqual "for initial alternate output," 8 cV
    xI $ set 7
    yI $ set 13
    (_, bV) <- bO
    lift $ assertEqual "for second output," 14 bV
    zI $ set y
    xI $ set 3
    (_, cV) <- cO
    lift $ assertEqual "for second alternate output," 6 cV
    (_, bV) <- bO
    lift $ assertEqual "for third output," 26 bV
    yI $ set 4
    (_, bV) <- bO
    lift $ assertEqual "for fourth output," 8 bV
    zI $ set x
    yI $ set 12
    (_, bV) <- bO
    lift $ assertEqual "for last output," 6 bV

stressTest :: IO ()
stressTest = runReactT $ do
    inputs <- mapM input (take 100 $ repeat (1 :: Integer))
    let pyramid xs = case xs of
          (x : y : xs) ->
            let z = cache ((+) <$> x <*> y)
            in z : pyramid (y : xs)
          _ -> []
    let [final] = (iterate pyramid $ map fst inputs) !! 99
    (fV, fO) <- output final
    lift $ assertEqual "for initial output," (2 ^ 99) fV
    snd (inputs !! 1) $ set 2
    (_, fV) <- fO
    lift $ assertEqual "for second output," (2 ^ 99 + 99) fV
    forM inputs (\(_, i) -> i $ set 5)
    (_, fV) <- fO
    lift $ assertEqual "for third output," (5 * 2 ^ 99) fV

tests :: Test
tests = test [
    "map" ~: test mapTest,
    "defer" ~: test deferTest,
    "stress" ~: test stressTest]
