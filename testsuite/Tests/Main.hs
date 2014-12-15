module Main (
    main,
    tests
) where

import Test.HUnit
import qualified Tests.Signal
import System.Exit

main :: IO ()
main = do
    res <- runTestTT tests
    if errors res > 0 || failures res > 0 then exitFailure else exitSuccess

tests :: Test
tests = test ["signal" ~: Tests.Signal.tests]
