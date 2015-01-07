{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import System.Console.ANSI
import Terminal.Widget hiding (await)
import qualified Terminal.UI as UI
import Control.Monad (replicateM_)

main :: IO ()
main =
    let ?style = UI.casino
    in runWidget undefined undefined UI.main

reset :: IO ()
reset = do
    setSGR [Reset]
    replicateM_ 30 $ putStrLn ""
