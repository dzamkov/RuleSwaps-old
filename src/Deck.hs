{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Deck (
    card,
    standard
) where

import Prelude hiding (take)
import Base
import Prim
import Game hiding (Prim)
import qualified Game
import Data.Typeable
import Data.Maybe (fromMaybe)

-- | Shorthand for describing a card.
card :: (Game.Prim g ~ Prim) => Type -> [Maybe BasePrim] -> CardInfo g
card t list = raiseType t $ \(_ :: Proxy r) -> CardInfo
    ((fromMaybe (error "bad card") $ listToTerm list) :: Term Prim Abs r)

-- Shorthand for this next part
slot = Nothing
p = Just

-- | The standard deck.
standard :: (Game.Prim g ~ Prim) => DeckInfo g
standard = [

    -- Drawing cards
    (1, card Action [p PlyDrawN, slot, p (Const 6)]),
    (3, card Action [p PlyDrawN, slot, p (Const 3)]),
    (4, card Action [p PlyDrawN, slot, p (Const 2)]),
    (3, card Action [p PlyDrawN, slot, p (Const 1)]),

    -- Banking cards
    (4, card Action [p PlyGainCoin, slot, slot]),
    (4, card Action [p PlyLoseCoin, slot, slot]),

    -- Action utilities
    (10, card Action [p Then, slot, slot]),
    (8, card Action [p If, slot, slot]),

    -- Condition utilities
    (3, card Condition [p Always]),
    (4, card Condition [p Never]),

    -- Number constants
    (3, card Number [p (Const 0)]),
    (3, card Number [p (Const 1)])]
