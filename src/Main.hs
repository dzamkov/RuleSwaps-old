module Main where

import System.Console.ANSI
import Terminal.Figure

main :: IO ()
main = test $ text ((Dull, Black), (Vivid, Yellow)) $ take 360 $ cycle
    "blah blah blah  text blah blah  more text blah blah blah blah blah blah"
