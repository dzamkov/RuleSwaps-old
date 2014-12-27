module Main where

import System.Console.ANSI
import Terminal.Figure

fore1 = (Vivid, Cyan)
fore2 = (Vivid, Green)
back1 = (Dull, Yellow)
user name = tightText fore1 "<" +++ tightText fore2 name +++ tightText fore1 ">"
chat = user "dr420scopes" +++ (text (Dull, White) $ take 360 $ cycle
    " blah blah blah  text blah blah  more text blah blah blah blah blah blah")
chatbox = box ((Dull, Blue), (Vivid, White)) $ blockify (Dull, Magenta) chat

main :: IO ()
main = test (center back1 $ setWidth 22 chatbox)
