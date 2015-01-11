{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Base (
    Type (..),
    IsType (..),
    raiseType,
    TypeList (..),
    IsTypeList (..),
    Manner (..),
    Mix,
    Term (..),
    TermList (..),
    toConTerm
) where

import Data.Typeable
import Control.Applicative

-- | Identifies the type of a value within the game.
data Type
    = Action
    | Condition
    | Player
    | Number
    deriving (Eq, Ord, Show, Typeable)
deriving instance Typeable Action
deriving instance Typeable Player
deriving instance Typeable Condition
deriving instance Typeable Number

-- | @a@ is of kind 'Type'.
class Typeable a => IsType (a :: Type) where

    -- | Converts a type-level 'Type' to a value-level 'Type'.
    lowerType :: Proxy a -> Type
instance IsType Action where
    lowerType _ = Action
instance IsType Condition where
    lowerType _ = Condition
instance IsType Player where
    lowerType _ = Player
instance IsType Number where
    lowerType _ = Number

-- | Converts a value-level 'Type' to a type-level 'Type'.
raiseType :: Type -> (forall a. (IsType a) => Proxy a -> r) -> r
raiseType Action inner = inner (Proxy :: Proxy Action)
raiseType Condition inner = inner (Proxy :: Proxy Condition)
raiseType Player inner = inner (Proxy :: Proxy Player)
raiseType Number inner = inner (Proxy :: Proxy Number)

-- | A list of 'Type's.
data TypeList
    = Nil
    | (:>:) Type TypeList
infixr :>:
deriving instance Typeable Nil
deriving instance Typeable (:>:)

-- | @a@ is of kind 'TypeList'.
class Typeable a => IsTypeList (a :: TypeList) where

    -- | Constructs a 'TermList' using a 'TypeList' as a template.
    mkTermList :: (Applicative f) => Proxy a
        -> (forall b. (IsType b) => f (Term p m b))
        -> f (TermList p m a)

instance IsTypeList Nil where
    mkTermList _ _ = pure TNil
instance (IsType a, IsTypeList b) => IsTypeList (a :>: b) where
    mkTermList _ inner = TCons <$> inner <*>
        mkTermList (Proxy :: Proxy b) inner

-- | Specifies whether a term has slots ('Abs') or not ('Con').
data Manner = Abs | Con

-- | The 'Manner' for a term consisting of terms of the given manners.
type family Mix (x :: Manner) (y :: Manner) :: Manner where
    Mix Abs y = Abs
    Mix x Abs = Abs
    Mix Con y = y
    Mix x Con = x
    Mix m m = m

-- | A composition of primitives that yields a value of type @r@. When abstract
-- (@m ~ Abs@), the term may contain "slots" which are placeholders for values.
data Term (p :: TypeList -> Type -> *) (m :: Manner) (r :: Type) where

    -- | A placeholder for a value.
    Slot :: Term p Abs r

    -- | An application of a primitive to a list of terms representing
    -- arguments.
    App :: p a r -> TermList p m a -> Term p m r

-- | A list of terms corresponding to a 'TypeList'. This is used to implement
-- 'Term'.
data TermList (p :: TypeList -> Type -> *) (m :: Manner) (t :: TypeList) where
    TNil :: TermList p m Nil
    TCons :: Term p x a -> TermList p y b -> TermList p (Mix x y) (a :>: b)

-- | Tries converting a term into a concrete term.
toConTerm :: Term p m r -> Maybe (Term p Con r)
toConTerm Slot = Nothing
toConTerm (App prim list) = App prim <$> toConList list where
    toConList :: TermList p m r -> Maybe (TermList p Con r)
    toConList TNil = Just TNil
    toConList (TCons a b) = TCons <$> toConTerm a <*> toConList b
