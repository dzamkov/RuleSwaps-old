{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Deck
import Reactive
import qualified Reactive.IO
import Markup hiding (Flow, Block, Widget)
import Markup.Builder
import Terminal.Base
import Terminal.Flow (Flow)
import Terminal.Block (Block, place)
import qualified Terminal.Block as Block
import Terminal.Draw
import Terminal.Paint
import Terminal.Input
import Terminal.Widget
import qualified System.Console.ANSI as ANSI
import Data.Monoid
import Control.Monad.Identity
import Control.Applicative

testFlow :: (Reactive e f) => Widget e f Flow ()
testFlow = text (color $ Color ANSI.Vivid ANSI.Green) $
    take 450 $ cycle "a ab abc "

testBlock :: (Reactive e f) => Widget e f Block ()
testBlock = res where
    green = Color ANSI.Vivid ANSI.Green
    red = Color ANSI.Vivid ANSI.Red
    blue = Color ANSI.Vivid ANSI.Blue
    res = setBack (Color ANSI.Dull ANSI.Red) $ setWidth 29 (
        solid red |||
        setHeight 6 (blockify Center testFlow) |||
        solid blue)

main = runWidget testBlock

reset :: IO ()
reset = do
    ANSI.setSGR [ANSI.Reset]
    replicateM_ 30 $ putStrLn ""
