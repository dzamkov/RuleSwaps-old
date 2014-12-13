module Main where

import System.Console.ANSI
import Terminal.Figure

back = (Dull, Black)
appr1 = (back, (Vivid, Cyan))
appr2 = (back, (Vivid, Green))
user name = tightText appr1 "<" +++ tightText appr2 name +++ tightText appr1 ">"

main :: IO ()
main = test $ user "dr420scopes" +++ (text ((Dull, Black), (Dull, White)) $ take 360 $ cycle
    " blah blah blah  text blah blah  more text blah blah blah blah blah blah")
