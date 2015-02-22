{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Deck
import Markup hiding (Flow, Block)
import Terminal.Context
import Terminal.Flow (Flow)
import Terminal.Block (Block, place)
import qualified Terminal.Block as Block
import Terminal.Draw
import Terminal.Paint
import Terminal.Input
import qualified System.Console.ANSI as ANSI
import Reactive.Banana hiding (Identity)
import Reactive.Banana.Frameworks
import Data.Monoid
import Control.Monad.Identity
import Control.Applicative

testFlow :: Flow Identity
testFlow = text Font (Color ANSI.Vivid ANSI.Green) $
    take 35 $ cycle "a ab abc "

testBlock :: Block Identity
testBlock = res where
    green = Color ANSI.Vivid ANSI.Green
    red = Color ANSI.Vivid ANSI.Red
    blue = Color ANSI.Vivid ANSI.Blue
    square c = setBack c clear
    res = setWidth 29 (
        square red |||
        setHeight 6 (blockify Center testFlow) |||
        square green)

main :: IO ()
main = do
    let back = Color ANSI.Dull ANSI.Magenta
    let (width', height', paint) = place testBlock width' height'
    let height = runIdentity height'
    let draw = runIdentity $ fromPaint $ paint $ pure (Just back, (0, 0))
    runDrawInline height draw

{-
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
-}

reset :: IO ()
reset = do
    ANSI.setSGR [ANSI.Reset]
    replicateM_ 30 $ putStrLn ""
