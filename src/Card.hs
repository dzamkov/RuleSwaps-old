{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Card (
    Card,
    getCardTerm,
    deck
) where

import Game
import Prim

-- | Describes a possible card.
newtype Card = Card (Term Prim ()) deriving (Eq, Ord, Show)

-- | Gets the term associated with a card. This fully describes the behavior
-- of the card.
getCardTerm :: Card -> Term Prim ()
getCardTerm (Card term) = term

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
deck :: [(Card, Integer)]
deck = [

    -- Drawing cards
    (Card $ app2 PPlyDrawN ply (app0 $ PConst 6), 2),
    (Card $ app2 PPlyDrawN ply (app0 $ PConst 3), 6),
    (Card $ app2 PPlyDrawN ply (app0 $ PConst 2), 12),
    (Card $ app2 PPlyDrawN ply (app0 $ PConst 1), 6),

    -- Banking cards
    (Card $ app2 PPlyGainCoin ply num, 9),
    (Card $ app2 PPlyLoseCoin ply num, 9),

    -- Action utilities
    (Card $ app2 PThen act act, 18),
    (Card $ app2 PIf con act, 8),

    -- Condition utilities
    (Card $ app0 PAlways, 6),
    (Card $ app0 PNever, 6),

    -- Number constants
    (Card $ app0 $ PConst 0, 3),
    (Card $ app0 $ PConst 1, 5),
    (Card $ app0 $ PConst 2, 4),
    (Card $ app0 $ PConst 3, 3),
    (Card $ app0 $ PConst 4, 2),
    (Card $ app0 $ PConst 5, 2),
    (Card $ app0 $ PConst 6, 2),
    (Card $ app0 $ PConst 7, 2),
    (Card $ app0 $ PConst 8, 1),
    (Card $ app0 $ PConst 9, 1),
    (Card $ app0 $ PConst 10, 2),
    (Card $ app0 $ PConst 25, 1),
    (Card $ app0 $ PConst 30, 1),
    (Card $ app0 $ PConst 50, 1),
    (Card $ app0 $ PConst 100, 1)]
