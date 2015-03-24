{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Reactive.IO
import Terminal.Widget (runWidget)
import qualified System.Console.ANSI as ANSI
import qualified UI
import Control.Monad (replicateM_)

main = do
    (running, changeRunning) <- newBehavior True
    let ?style = UI.Casino
    let input = undefined
    output <- runWidget running (UI.main input)
    await (UI.quit output)
    changeRunning $ const False

reset :: IO ()
reset = do
    ANSI.setSGR [ANSI.Reset]
    replicateM_ 30 $ putStrLn ""
