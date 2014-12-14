module Tests.Signal (
    tests
) where

import Prelude hiding (map)
import Delta
import Signal
import Test.HUnit
import Control.Monad.Trans (lift)

mapTest :: IO ()
mapTest = runReactT proc where
    proc :: ReactT r IO ()
    proc = do
        (xI, x) <- input (5 :: Int)
        let y = map (deltaMap (+ 3)) x
        (yO, yV) <- output y
        lift $ assertEqual "for initial output," 8 yV
        (yD, yV) <- look yO
        lift $ assertEqual "for initial look delta," keep yD
        lift $ assertEqual "for initial look value," 8 yV
        update xI (set 8)
        (yD, yV) <- look yO
        lift $ assertEqual "for updated look delta," (set 11) yD
        lift $ assertEqual "for updated look value," 11 yV
        update xI keep
        (_, yV) <- look yO
        lift $ assertEqual "for pseudo-updated look value," 11 yV

tests :: Test
tests = test ["map" ~: test mapTest]
