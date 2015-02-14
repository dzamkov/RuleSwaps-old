{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Deck
import Markup hiding (Flow)
import Terminal.Context
import Terminal.Flow
import Terminal.Draw
import Terminal.Paint
import Terminal.Input
import qualified System.Console.ANSI as ANSI
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad.Identity
import Control.Applicative

main :: IO ()
main = do
    (keyAddHandler, onKey) <- newAddHandler
    network <- compile $ mdo
        key <- fromChanges ' ' keyAddHandler
        let appr = (Color ANSI.Dull ANSI.Magenta, Color ANSI.Vivid ANSI.Green)
        let paintTest = foldl1 mix [
                toPaint $ pure $ string appr (0, 0) "Last key was:",
                toPaint $ string appr (0, 1) . (: []) <$> key,
                toPaint $ pure $ string appr (0, 3) "Terminal size is:",
                toPaint $ string appr (0, 4) . show <$> size]
        size <- runPaint paintTest
        return ()
    actuate network
    let listenKey = do
        ch <- getHiddenChar
        onKey ch
        unless (ch == 'q') listenKey
    listenKey

{-
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
-}

reset :: IO ()
reset = do
    ANSI.setSGR [ANSI.Reset]
    replicateM_ 30 $ putStrLn ""
