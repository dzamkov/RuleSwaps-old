{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Prim (
    BasePrim (..),
    Prim (..),
    AnyPrim (..),
    toPrim,
    termToList,
    listToTerm,
    conTermToList,
    conListToTerm
) where

import Prelude hiding (print)
import Base
import Game hiding (Player, Prim)
import qualified Game
import Data.Typeable
import Data.Maybe (fromJust)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad (when, void, mzero)
import Control.Applicative hiding (Const)

-- | Identifiers for primitive operations in a game, without associated type
-- information.
data BasePrim
    = PlyDrawN
    | PlyGainCoin
    | PlyLoseCoin
    | Then | If
    | Always | Never
    | Const Integer
    deriving (Eq, Ord, Show)

-- | Describes a primitive operation in a game, with associated type
-- information.
data Prim (a :: [Type]) (r :: Type) = Prim {

    -- | Converts a typed primitive to a base primitive.
    toBasePrim :: BasePrim,

    -- | Gets the game procedure needed to run a primitive operation.
    runPrim :: forall (f :: Type -> *) g.
        (forall t. (IsType t) => f t -> Game g (Game.Value g t))
        -> FList f a -> Game g (Game.Value g r),

    -- | A list of strings used to display the primitive to a user. These
    -- strings fill the space around arguments when displayed. There should be
    -- one more string than arguments.
    text :: [String] }
    deriving (Typeable)

-- | A primitive of any argument and result type.
data AnyPrim = forall a r. (IsTypeList a, IsType r) => AnyPrim (Prim a r)
    deriving (Typeable)

-- | Simple proxy used to specify argument and return types in 'mkPrim'.
data T (a :: [Type]) (r :: Type) = T

-- | Helper for implementing 'toPrim'.
mkPrim :: forall (a :: [Type]) (r :: Type). (IsTypeList a, IsType r)
    => BasePrim
    -> [String] -> T a r
    -> (forall (f :: Type -> *) g.
        (forall t. (IsType t) => f t -> Game g (Game.Value g t))
        -> FList f a -> Game g (Game.Value g r))
    -> AnyPrim
mkPrim toBasePrim text _ runPrim = AnyPrim (Prim { .. } :: Prim a r)

-- | Converts a 'BasePrim' into its associated typed primitive.
toPrim :: BasePrim -> AnyPrim
toPrim PlyDrawN = mkPrim PlyDrawN
    ["", " draws ", " cards"]
    (T :: T '[Player, Number] Action) $
    \eval (FCons ply (FCons num FNil)) -> do
        Value ply <- eval ply
        Value num <- eval num
        error "implement PlyDrawN" -- TODO
        return $ Value ()
toPrim PlyGainCoin = mkPrim PlyGainCoin
    ["", " gains ", " coins"]
    (T :: T '[Player, Number] Action) $
    \eval (FCons ply (FCons num FNil)) -> do
        Value ply <- eval ply
        Value num <- eval num
        when (num > 0) $ do
            bank ply num
            print $ msgPlayer ply <~> msg "gained" <~> msgCoins num
        return $ Value ()
toPrim PlyLoseCoin = mkPrim PlyLoseCoin
    ["", " loses ", " coins"]
    (T :: T '[Player, Number] Action) $
    \eval (FCons ply (FCons num FNil)) -> do
        Value ply <- eval ply
        Value num <- eval num
        wealth <- getWealth ply
        let lost = min wealth num
        when (lost > 0) $ do
            bank ply (-lost)
            print $ msgPlayer ply <~> msg "lost" <~> msgCoins lost
        return $ Value ()
toPrim Then = mkPrim Then
    ["", ", then ", ""]
    (T :: T '[Action, Action] Action) $
    \eval (FCons x (FCons y FNil)) -> do
        void $ eval x
        void $ eval y
        return $ Value ()
toPrim If = mkPrim If
    ["if", ", then ", ""]
    (T :: T '[Condition, Action] Action) $
    \eval (FCons cond (FCons act FNil)) -> do
        Value okay <- eval cond
        when okay $ void $ eval act
        return $ Value ()
toPrim Always = mkPrim Always
    ["always"]
    (T :: T '[] Condition) $
    \_ FNil -> return $ Value True
toPrim Never = mkPrim Never
    ["never"]
    (T :: T '[] Condition) $
    \_ FNil -> return $ Value False
toPrim (Const v) = mkPrim (Const v)
    [show v]
    (T :: T '[] Number) $
    \_ FNil -> return $ Value v

-- | Converts a term into a list of 'BasePrim's using a depth-first traversal.
-- 'Nothing' is used to represent slots. Use 'listToTerm' to invert.
termToList :: Term Prim m r -> [Maybe BasePrim]
termToList Slot = [Nothing]
termToList (App prim args) = Just (toBasePrim prim) : argsToList args where
    argsToList :: FList (Term Prim m) a -> [Maybe BasePrim]
    argsToList FNil = []
    argsToList (FCons a as) = termToList a ++ argsToList as

-- | Converts a concrete term into a list of 'BasePrim's using a depth-first
-- traversal. Use 'conListToTerm' to invert.
conTermToList :: Term Prim Con r -> [BasePrim]
conTermToList term = fromJust <$> termToList term

-- | Tries converting a list of 'BasePrim's into a well-typed term.
-- Use 'termToList' to invert.
listToTerm :: (IsType r) => [Maybe BasePrim] -> Maybe (Term Prim Abs r)
listToTerm list = res where
    take :: StateT [Maybe BasePrim] Maybe (Maybe BasePrim)
    take = do
        xs <- get
        case xs of
            [] -> mzero
            x : xs -> put xs >> return x
    parse :: forall r. (IsType r)
        => StateT [Maybe BasePrim] Maybe (Term Prim Abs r)
    parse = do
        sym <- take
        case sym of
            Nothing -> return Slot
            Just basePrim -> case toPrim basePrim of
                AnyPrim prim -> parseApp prim
    parseApp :: forall a r r'. (IsTypeList a, IsType r, IsType r')
        => Prim a r' -> StateT [Maybe BasePrim] Maybe (Term Prim Abs r)
    parseApp prim = case eqT :: Maybe (r :~: r') of
        Nothing -> mzero
        Just Refl -> App prim <$> mkTermList (Proxy :: Proxy a) parse
    res = case runStateT parse list of
        Just (res, []) -> Just res
        _ -> Nothing

-- | Tries converting a list of 'BasePrim's into a well-typed concrete term.
-- Use 'conListToTerm' to invert.
conListToTerm :: (IsType r) => [BasePrim] -> Maybe (Term Prim Con r)
conListToTerm list = listToTerm (Just <$> list) >>= toConTerm

-- Useful instance is useful
instance Show (Term Prim m r) where
    showsPrec n term = showParen (n > 0)
        (("fromMaybe $ listToTerm " ++) . shows (termToList term))
