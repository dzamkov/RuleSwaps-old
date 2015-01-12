{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import Actor
import Deck
import System.Console.ANSI
import Terminal.Widget
import qualified Terminal.UI as UI
import Control.Monad (replicateM_)

main :: IO ()
main = runActorIO $ do
    quitChan <- spawn
    stopTerminalChan <- spawn
    exitChan <- spawn
    fork $ do
        await $ source quitChan
        send stopTerminalChan ()
        send exitChan ()
    (global, inst) <- startTerminal $ source stopTerminalChan
    let ?style = UI.casino
    let context = UI.MainContext {
        UI.quit = send quitChan () }
    fork $ liftActor $ runWidget global inst $ UI.main context
    await $ source exitChan

reset :: IO ()
reset = do
    setSGR [Reset]
    replicateM_ 30 $ putStrLn ""
