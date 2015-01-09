{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import Actor
import System.Console.ANSI
import Terminal.Widget
import qualified Terminal.UI as UI
import Control.Monad (replicateM_)

main :: IO ()
main = runActorIO $ do
    exitChan <- spawn
    (global, inst) <- startTerminal $ source exitChan
    let ?style = UI.casino
    let context = UI.MainContext {
        UI.exit = yield exitChan () }
    liftActor $ runWidget global inst $ UI.main context

reset :: IO ()
reset = do
    setSGR [Reset]
    replicateM_ 30 $ putStrLn ""
