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
import Terminal.Widget
import qualified System.Console.ANSI as ANSI
import Reactive.Banana hiding (Identity)
import Reactive.Banana.Frameworks
import Data.Monoid
import Control.Monad.Identity
import Control.Applicative

testFlow :: Widget t Flow ()
testFlow = text Font (Color ANSI.Vivid ANSI.Green) $
    take 20 $ cycle "a ab abc "

{- testBlock :: Widget t Block ()
testBlock = res where
    green = Color ANSI.Vivid ANSI.Green
    red = Color ANSI.Vivid ANSI.Red
    blue = Color ANSI.Vivid ANSI.Blue
    square c = setBack c clear
    res = setWidth 29 (
        square red |||
        setHeight 6 (blockify Center testFlow) |||
        square green) -}

testBlock :: Widget t Block ()
testBlock = setBack (Color ANSI.Dull ANSI.Blue) (blockify Center testFlow)

main = do
    network <- compile $ runWidget testBlock
    actuate network

reset :: IO ()
reset = do
    ANSI.setSGR [ANSI.Reset]
    replicateM_ 30 $ putStrLn ""
