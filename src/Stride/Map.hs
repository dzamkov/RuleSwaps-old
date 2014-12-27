{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
module Stride.Map (
    CMap,
    dunion
) where

import Stride
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative

-- Define complex stride for map.
data CMap k v = CMap (Map k v) (Map k v) (Map k ())
type instance Complex (Map k v) = CMap k v
instance Ord k => IsStride (CMap k v) where
    glue (CMap o aa ad) (CMap _ ba bd) = CMap o
        (Map.union ba (Map.difference aa bd))
        (Map.union (Map.difference ad ba) bd)
instance Ord k => StrideRel (CMap k v) (Map k v) where
    start (CMap o _ _) = o
    end (CMap o a d) = Map.union a (Map.difference o d)

-- | Computes the left-biased union of two maps within the context of a deltor.
dunion :: (Deltor f, Ord k) => f (Map k v) -> f (Map k v) -> f (Map k v)
dunion a b = deltor (\eval -> union <$> eval a <*> eval b) where
    toCMap (Stride a b) = CMap a (Map.difference b a)
        (Map.map (const ()) $ Map.difference a b)
    toCMap (Stay x) = CMap x Map.empty Map.empty
    toCMap (Complex x) = x
    u = Map.union
    d = Map.difference
    i = Map.intersection
    union (Stay x) (Stay y) = Stay $ u x y
    union (toCMap -> CMap xo xa xd) (toCMap -> CMap yo ya yd) = Complex $
        CMap (u xo yo) (u xa (d ya xo))
        (u (d (d yd xo) xa) $ u (d (d xd yo) ya) $ i xd yd)
