{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import Actor
import Deck
import Markup hiding (Flow)
import Terminal.Context
import Terminal.Flow
import Terminal.Draw
import Terminal.Paint
import qualified System.Console.ANSI as ANSI
import Control.Monad.Identity
import Control.Applicative

test :: Flow Identity
test = text Font (Color ANSI.Vivid ANSI.Green) $
    take 500 $ cycle "testing the flow because its good to be "

main :: IO ()
main = do
    let back = Color ANSI.Dull ANSI.Magenta
    let (height', paint) = placeFlow test center (pure 50)
    let height = runIdentity height'
    let draw = runIdentity $ fromPaint $ paint $ pure (back, (0, 0))
    runDrawInline height draw

{-
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
-}
