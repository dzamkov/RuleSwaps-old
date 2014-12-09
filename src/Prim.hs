{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prim (
    Prim (..),
    primitiveText
) where

import Game
import Control.Monad

-- | Contains a set of fun primitives for a game.
data Prim
    = PPlyDrawN
    | PPlyGainCoin
    | PPlyLoseCoin
    | PThen
    | PIf
    | PAlways
    | PNever
    | PConst Integer
    deriving (Eq, Ord, Show)

playerId p = do
    p' <- p
    return $ getValue p'
playerInfo p = do
    pId <- playerId p
    s <- state
    return (pId, getPlayer pId s)
numberValue :: Game p h Value -> Game p h Integer
numberValue = fmap getValue
instance Primitive Prim where
    primitiveType PPlyDrawN = ([TPlayer, TNumber], TAction)
    primitiveType PPlyGainCoin = ([TPlayer, TNumber], TAction)
    primitiveType PPlyLoseCoin = ([TPlayer, TNumber], TAction)
    primitiveType PThen = ([TAction, TAction], TAction)
    primitiveType PIf = ([TCondition, TAction], TAction)
    primitiveType PAlways = ([], TCondition)
    primitiveType PNever = ([], TCondition)
    primitiveType (PConst _) = ([], TNumber)
    runPrimitive PPlyDrawN [p, n] = do
        pId <- playerId p
        nNum <- numberValue n
        drawN pId nNum
        return actValue
    runPrimitive PPlyGainCoin [p, n] = do
        (pId, pInfo) <- playerInfo p
        nNum <- numberValue n
        when (nNum > 0) $ do
            gainCoins pId nNum
            message (name pInfo ++ " gains " ++ show nNum ++
                (if nNum == 1 then " coin" else " coins"))
        return actValue
    runPrimitive PPlyLoseCoin [p, n] = do
        (pId, pInfo) <- playerInfo p
        nNum <- numberValue n
        when (nNum > 0) $ do
            let rNum = min (coins pInfo) nNum
            loseCoins pId rNum
            message (name pInfo ++ " loses " ++ show nNum ++
                (if nNum == 1 then " coin" else " coins"))
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
    runPrimitive PAlways [] = return $ mkValue True
    runPrimitive PNever [] = return $ mkValue False
    runPrimitive (PConst v) [] = return $ mkValue v
    runPrimitive _ _ = error "Wrong number of arguments to primitive"

-- | Gets a list of strings used to display a primitive to a user. These
-- strings fill in the space around slots when displayed.
primitiveText :: Prim -> [String]
primitiveText PPlyDrawN = ["", " draws ", " cards"]
primitiveText PPlyGainCoin = ["", " gains ", " coins"]
primitiveText PPlyLoseCoin = ["", " loses ", " coins"]
primitiveText PThen = ["", ", then " , ""]
primitiveText PIf = ["if ", ", then ", ""]
primitiveText PAlways = ["always"]
primitiveText PNever = ["never"]
primitiveText (PConst v) = [show v]
