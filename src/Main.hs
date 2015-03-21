{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Deck
import Reactive
import qualified Reactive.IO
import Markup hiding (Flow, Block, Widget)
import Markup.Builder
import Markup.Attr
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

testFlow :: (Reactive e f) => Widget e f Flow (e ())
testFlow = runBuilder $ do
    let pre = text (color $ Color ANSI.Vivid ANSI.Green) $
            take 450 $ cycle "a ab abc "
    (agree, agreeE) <- use $ button (key 'A' . title "Agree")
    return (pre <> agree, agreeE)

testBlock :: (Reactive e f) => Widget e f Block (e ())
testBlock = runBuilder $ do
    let green = Color ANSI.Vivid ANSI.Green
    let red = Color ANSI.Vivid ANSI.Red
    let blue = Color ANSI.Vivid ANSI.Blue
    (flow, flowE) <- use $ testFlow
    let res = setBack (Color ANSI.Dull ANSI.Red) $ setWidth 29
            (solid red |||
            setHeight 6 (blockify Center flow) |||
            solid blue)
    return (res, flowE)

main = runWidget testBlock

reset :: IO ()
reset = do
    ANSI.setSGR [ANSI.Reset]
    replicateM_ 30 $ putStrLn ""
