{-# LANGUAGE TypeFamilies #-}
module Main where

import Record (Record, EnumRecord)
import System.Console.ANSI
import Terminal.Figure
import Terminal.Page
import Terminal.Widget
import Control.Applicative hiding (empty)

data MenuOptions = Join | Host | Exit deriving (Eq, Ord, Enum, Bounded)
type instance Record MenuOptions = EnumRecord MenuOptions

keyColor = (Vivid, Blue)
headerColor = (Vivid, Red)
textColor = (Dull, Black)
highlightColor = (Vivid, Yellow)
screenBack = (Dull, Green)
menuBorder = (Dull, White)
menuBack = (Vivid, White)

menuOption id keys name = res where
    view = keyView (\keys -> case keys id of
        Just key -> tightText keyColor ('[' : key : ']' : []) +++ space 1
        Nothing -> empty)
    space' = figureToPage . space
    label = figureToPage $ text textColor name
    res = option id keys (highlightFlow highlightColor) $
        (view +++ label +++ space' 1)

menuList =
    figureToPage (tightText headerColor "RuleSwaps")
    === figureToPage empty ===
    menuOption Join ['j'] "Join"
    ===
    menuOption Host ['h'] "Host"
    ===
    menuOption Exit ['e'] "Exit"

menuBox =
    -- box ((Dull, Black), (Vivid, Cyan)) $
    pad menuBorder (1, 1, 1, 1) $
    pad menuBack (2, 1, 2, 1) $
    setWidth 10 $
    blockify menuBack menuList

menuPage = center $ pad screenBack (9, 9, 9, 9) $ menuBox

main :: IO ()
main = runWidget undefined undefined (pageToWidget menuPage)
