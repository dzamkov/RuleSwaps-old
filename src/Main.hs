module Main where

import Signal
import System.Console.ANSI
import Terminal.Figure
import Terminal.Widget
import Control.Applicative

fore1 = (Vivid, Cyan)
fore2 = (Vivid, Green)
back1 = (Dull, Yellow)
user name = tightText fore1 "<" +++ tightText fore2 name +++ tightText fore1 ">"
chat = user "dr420scopes" +++ (text (Dull, White) $ take 360 $ cycle
    " blah blah blah  text blah blah  more text blah blah blah blah blah blah")
chatbox = box ((Dull, Blue), (Vivid, White)) $ blockify (Dull, Magenta) chat
widget = figure $ center $ setWidth 22 chatbox

main :: IO ()
main = runReactT $ runWidget (undefined :: Int -> ReactT r IO ()) (pure widget)
