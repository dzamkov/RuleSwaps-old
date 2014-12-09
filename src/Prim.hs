{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prim (
    Prim (..),
    primitiveText
) where

import Game

-- | Contains a set of fun primitives for a game.
data Prim
    = PPlyDrawN
    | PPlyGainCoin
    | PPlyLoseCoin
    | PThen
    | PIf
    deriving (Eq, Ord, Show)

actValue :: Value
actValue = mkValue ()
instance Primitive Prim where
    primitiveType PPlyDrawN = ([TPlayer, TNumber], TAction)
    primitiveType PPlyGainCoin = ([TPlayer, TNumber], TAction)
    primitiveType PPlyLoseCoin = ([TPlayer, TNumber], TAction)
    primitiveType PThen = ([TAction, TAction], TAction)
    primitiveType PIf = ([TCondition, TAction], TAction)
    runPrimitive PPlyDrawN [p, n] = do
        p' <- p
        n' <- n
        drawN (getValue p') (getValue n')
        return actValue
    runPrimitive PPlyGainCoin [p, n] = do
        p' <- p
        n' <- n
        bank (getValue p') (getValue n')
        return actValue
    runPrimitive PPlyLoseCoin [p, n] = do
        p' <- p
        n' <- n
        bank (getValue p') (negate $ getValue n')
        return actValue
    runPrimitive PThen [a, b] = do
        _ <- a
        _ <- b
        return actValue
    runPrimitive PIf [c, a] = do
        c' <- c
        if getValue c'
            then do
                _ <- a
                return actValue
            else return actValue
    runPrimitive _ _ = error "Wrong number of arguments to primitive"

-- | Gets a list of strings used to display a primitive to a user. These
-- strings fill in the space around slots when displayed.
primitiveText :: Prim -> [String]
primitiveText PPlyDrawN = ["", " draws ", " cards"]
primitiveText PPlyGainCoin = ["", " gains ", " coins"]
primitiveText PPlyLoseCoin = ["", " loses ", " coins"]
primitiveText PThen = ["", ", then " , ""]
primitiveText PIf = ["if ", ", then ", ""]
