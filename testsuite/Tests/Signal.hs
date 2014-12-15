module Tests.Signal (
    tests
) where

import Delta
import Signal hiding (map)
import qualified Signal
import Test.HUnit
import Control.Monad.Trans (lift)

mapTest :: IO ()
mapTest = runReactT $ do
    (xI, x) <- input (5 :: Int)
    let y = Signal.map (deltaMap (+ 3)) x
    (yO, yV) <- output y
    lift $ assertEqual "for initial output," 8 yV
    (yD, yV) <- look yO
    lift $ assertEqual "for initial look delta," keep yD
    lift $ assertEqual "for initial look value," 8 yV
    update xI $ set 8
    (yD, yV) <- look yO
    lift $ assertEqual "for updated look delta," (set 11) yD
    lift $ assertEqual "for updated look value," 11 yV
    update xI keep
    (_, yV) <- look yO
    lift $ assertEqual "for pseudo-updated look value," 11 yV

deferTest :: IO ()
deferTest = runReactT $ do
    (xI, x) <- input (5 :: Int)
    (yI, y) <- input (8 :: Int)
    (zI, z) <- input x
    let a = Signal.defer z
    let b = Signal.map (deltaMap (* 2)) a
    let c = Signal.map (deltaMap (+ 3)) x
    (bO, bV) <- output b
    (cO, cV) <- output c
    lift $ assertEqual "for initial output," 10 bV
    lift $ assertEqual "for initial alternate output," 8 cV
    update xI $ set 7
    update yI $ set 13
    (_, bV) <- look bO
    lift $ assertEqual "for second output," 14 bV
    update zI $ set y
    update xI $ set 3
    (_, cV) <- look cO
    lift $ assertEqual "for second alternate output," 6 cV
    (_, bV) <- look bO
    lift $ assertEqual "for third output," 26 bV
    update yI $ set 4
    (_, bV) <- look bO
    lift $ assertEqual "for fourth output," 8 bV
    update zI $ set x
    update yI $ set 12
    (_, bV) <- look bO
    lift $ assertEqual "for last output," 6 bV

tests :: Test
tests = test [
    "map" ~: test mapTest,
    "defer" ~: test deferTest]
