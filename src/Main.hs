module Main where

import System.Console.ANSI
import Terminal.Figure

main :: IO ()
main = test $ tightText ((Dull, Black), (Vivid, Yellow))
    (take 600 $ cycle "abc")
