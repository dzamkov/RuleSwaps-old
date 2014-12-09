{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Card (
    deck
) where

import Game
import Prim

-- Shorthand functions for this next part
act = Slot TAction
con = Slot TCondition
ply = Slot TPlayer
num = Slot TNumber
app = App
app0 p = app p []
app1 p x = app p [((), x)]
app2 p x y = app p [((), x), ((), y)]
app3 p x y z = app p [((), x), ((), y), ((), z)]

-- | Lists all cards in a standard deck, paired with multiplicity.
deck :: [(Card Prim, Integer)]
deck = [

    -- Drawing cards
    (app2 PPlyDrawN ply (app0 $ PConst 6), 2),
    (app2 PPlyDrawN ply (app0 $ PConst 3), 6),
    (app2 PPlyDrawN ply (app0 $ PConst 2), 12),
    (app2 PPlyDrawN ply (app0 $ PConst 1), 6),

    -- Banking cards
    (app2 PPlyGainCoin ply num, 9),
    (app2 PPlyLoseCoin ply num, 9),

    -- Action utilities
    (app2 PThen act act, 18),
    (app2 PIf con act, 8),

    -- Condition utilities
    (app0 PAlways, 6),
    (app0 PNever, 6),

    -- Number constants
    (app0 $ PConst 0, 3),
    (app0 $ PConst 1, 5),
    (app0 $ PConst 2, 4),
    (app0 $ PConst 3, 3),
    (app0 $ PConst 4, 2),
    (app0 $ PConst 5, 2),
    (app0 $ PConst 6, 2),
    (app0 $ PConst 7, 2),
    (app0 $ PConst 8, 1),
    (app0 $ PConst 9, 1),
    (app0 $ PConst 10, 2),
    (app0 $ PConst 25, 1),
    (app0 $ PConst 30, 1),
    (app0 $ PConst 50, 1),
    (app0 $ PConst 100, 1)]
