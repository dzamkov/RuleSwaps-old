{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Deck
import Markup hiding (Flow, Block)
import Terminal.Context
import Terminal.Flow (Flow)
import Terminal.Block (Block, place)
import Terminal.Draw
import Terminal.Paint
import Terminal.Input
import qualified System.Console.ANSI as ANSI
import Reactive.Banana hiding (Identity)
import Reactive.Banana.Frameworks
import Control.Monad.Identity
import Control.Applicative

testFlow :: Flow Identity
testFlow = text Font (Color ANSI.Vivid ANSI.Green) $
    take 500 $ cycle "testing the flow because its good to be "

testBlock :: Block Identity
testBlock = res where
    green = Color ANSI.Vivid ANSI.Green
    red = Color ANSI.Vivid ANSI.Red
    blue = Color ANSI.Vivid ANSI.Blue
    square c = setBack c clear
    res = setWidth 10 $ setHeight 10 $
        ((square green ||| square blue) === square red)

main :: IO ()
main = do
    let back = Color ANSI.Dull ANSI.Magenta
    let (_, height', paint) = place testBlock undefined undefined
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
