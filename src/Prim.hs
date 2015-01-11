{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Base
import Data.Typeable
import Data.Maybe (fromJust)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad (mzero)
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
data Prim (a :: TypeList) (r :: Type) = Prim {

    -- | Converts a typed primitive to a base primitive.
    toBasePrim :: BasePrim,

    -- | A list of strings used to display the primitive to a user. These
    -- strings fill the space around arguments when displayed. There should be
    -- one more string than arguments.
    text :: [String] }
    deriving (Typeable)

-- | A primitive of any argument and result type.
data AnyPrim = forall a r. (IsTypeList a, IsType r) => AnyPrim (Prim a r)
    deriving (Typeable)

-- | Simple proxy used to specify argument and return types in 'mkPrim'.
data T (a :: TypeList) (r :: Type) = T

-- | Helper for implementing 'toPrim'.
mkPrim :: forall (a :: TypeList) (r :: Type). (IsTypeList a, IsType r)
    => BasePrim -> T a r
    -> [String] -> AnyPrim
mkPrim toBasePrim _ text = AnyPrim (Prim { .. } :: Prim a r)

-- | Converts a 'BasePrim' into its associated typed primitive.
toPrim :: BasePrim -> AnyPrim
toPrim PlyDrawN = mkPrim PlyDrawN
    (T :: T (Player :>: Number :>: Nil) Action)
    ["", " draws ", " cards"]
toPrim PlyGainCoin = mkPrim PlyGainCoin
    (T :: T (Player :>: Number :>: Nil) Action)
    ["", " gains ", " coins"]
toPrim PlyLoseCoin = mkPrim PlyLoseCoin
    (T :: T (Player :>: Number :>: Nil) Action)
    ["", " loses ", " coins"]
toPrim Then = mkPrim Then
    (T :: T (Action :>: Action :>: Nil) Action)
    ["", ", then ", ""]
toPrim If = mkPrim If
    (T :: T (Condition :>: Action :>: Nil) Action)
    ["if", ", then ", ""]
toPrim Always = mkPrim Always
    (T :: T Nil Condition)
    ["always"]
toPrim Never = mkPrim Never
    (T :: T Nil Condition)
    ["never"]
toPrim (Const v) = mkPrim (Const v)
    (T :: T Nil Number)
    [show v]

-- | Converts a term into a list of 'BasePrim's using a depth-first traversal.
-- 'Nothing' is used to represent slots. Use 'listToTerm' to invert.
termToList :: Term Prim m r -> [Maybe BasePrim]
termToList Slot = [Nothing]
termToList (App prim args) = Just (toBasePrim prim) : argsToList args where
    argsToList :: TermList Prim m a -> [Maybe BasePrim]
    argsToList TNil = []
    argsToList (TCons a as) = termToList a ++ argsToList as

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
